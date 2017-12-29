-- | Cache store paths and their references
module Nix.ReferenceCache (
  NixReferenceCache(..),
  newReferenceCache,
  initializeReferenceCache,
  getReferences,
  getReferencesIncludeSelf,
  recordReferences
  ) where

import Database.SQLite.Simple (Connection, Query, Only(..), lastInsertRowId)
import Database.SQLite.Simple (open, execute_, execute, query, query_)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Text as T
import System.Process.Text (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.Environment (lookupEnv)

import Nix.Cache.Common
import Nix.Cache.Client.Misc
import Nix.Bin (NixBinDir(..), getNixBinDir)
import Nix.StorePath (NixStoreDir(..), PathTree, PathSet, StorePath, spToText)
import Nix.StorePath (getNixStoreDir, spToFull, parseStorePath, spToPath)
import Nix.StorePath (ioParseFullStorePath)

-- | Figure out where to access the local nix state DB.
getNixDBDir :: IO FilePath
getNixDBDir = lookupEnv "NIX_DB_DIR" >>= \case
  Just dir -> pure dir
  Nothing -> lookupEnv "NIX_STATE_DIR" >>= \case
    Just stateDir -> pure (stateDir </> "nix" </> "db" </> "db.sqlite")
    Nothing -> do
      NixBinDir d <- getNixBinDir
      pure $ takeDirectory (takeDirectory (takeDirectory d))
         </> "var" </> "nix" </> "db" </> "db.sqlite"

-- | Figure out the location in which to put the cache.
getPathCacheLocation :: IO FilePath
getPathCacheLocation = lookupEnv "CLIENT_SQLITE_CACHE" >>= \case
  Just location -> pure location
  Nothing -> lookupEnv "HOME" >>= \case
    Nothing -> error "HOME variable isn't set"
    Just home -> pure (home </> ".nix-client-cache")

-- | Path reference cache, backed by sqlite
data NixReferenceCache = NixReferenceCache {
  nprcStoreDir :: NixStoreDir,
  -- ^ Location of the nix store.
  nprcLocalNixDbConnection :: Maybe Connection,
  -- ^ Connection to the local nix database (optional).
  nprcBinDir :: NixBinDir,
  -- ^ Location on disk for nix binaries.
  nprcCacheLocation :: FilePath,
  -- ^ Location of the cache SQLite database.
  nprcConnection :: MVar Connection,
  -- ^ Database connection for the local cache. Syncronized in MVar to
  -- allow lastrowid to be deterministic.
  nprcPathIdCache :: MVar (HashMap StorePath Int64),
  -- ^ Map store paths to their database row ID, so that we don't have to
  -- look them up all the time.
  nprcPathReferences :: MVar PathTree
  -- ^ Computed store path dependency tree.
  }

-- | Attempt to connect to the local SQLite nix database. If the
-- connection fails, or it doesn't have a ValidPaths table, return Nothing.
attemptLocalNixConnection :: FilePath -> IO (Maybe Connection)
attemptLocalNixConnection dbpath = do
  conn <- open dbpath
  let test = query_ conn "select count(*) from ValidPaths" :: IO [Only Int]
  (Just conn <$ test) `catch` \(err::SomeException) -> do
    putStrLn $ "Can't use local nix SQLite database: " <> tshow err
    pure Nothing

newReferenceCache :: IO NixReferenceCache
newReferenceCache = do
  nprcCacheLocation <- getPathCacheLocation
  nprcLocalNixDbConnection <- attemptLocalNixConnection =<< getNixDBDir
  nprcStoreDir <- getNixStoreDir
  nprcBinDir <- getNixBinDir
  nprcConnection <- newMVar =<< open nprcCacheLocation
  nprcPathIdCache <- newMVar mempty
  nprcPathReferences <- newMVar mempty
  pure NixReferenceCache {..}

-- | Get the references of an object by asking either a nix command or the DB.
-- This information is cached by the caller of this function.
getRefs :: NixReferenceCache -> StorePath -> IO PathSet
getRefs cache spath = case nprcLocalNixDbConnection cache of
  -- We can't access the DB directly. Use the CLI.
  Nothing -> getRefsFromNixCommand cache spath
  -- Pull the references directly out of the database.
  Just conn -> do
    -- Ensure it's in the nix DB by getting its ID
    nixPathId <- do
      let qry = "select id from ValidPaths where path = ?"
      query conn qry (Only $ spToText spath) >>= \case
        [Only pid] -> pure (pid :: Int64)
        _ -> error $ "Path " <> show spath <> " not stored in the nix DB"

    refs <- query conn getPathsQuery (Only nixPathId)
    map HS.fromList $ map snd <$> mapM ioParseFullStorePath (map fromOnly refs)

-- | Get references of a path by querying the nix CLI.
getRefsFromNixCommand :: NixReferenceCache -> StorePath -> IO PathSet
getRefsFromNixCommand cache spath = do
  let nixStoreCmd = unpackNixBinDir (nprcBinDir cache) </> "nix-store"
      storeDir = nprcStoreDir cache
  let args = ["--query", "--references", spToFull storeDir spath]
  readProcessWithExitCode nixStoreCmd args "" >>= \case
    (ExitSuccess, stdout, _) -> map HS.fromList $ do
      map snd <$> mapM ioParseFullStorePath (splitWS stdout)
    (ExitFailure code, _, stderr) -> error msg
      where cmd = nixStoreCmd <> " " <> intercalate " " args
            msg' = cmd <> " failed with " <> show code
            msg = msg' <> unpack (if T.strip stderr == "" then ""
                                  else "\nSTDERR:\n" <> stderr)

-- | Query which will return all of the references of a path.
getPathsQuery :: Query
getPathsQuery = fromString $ concat [
  "select path from ValidPaths inner join (",
  "select reference from ValidPaths inner join Refs on id = referrer where id = ?",
  ") on id = reference"
  ]

-- | Get the references of a path, checking and updating the cache.
-- Doesn't filter out self-references.
getReferencesIncludeSelf :: NixReferenceCache -> StorePath -> IO PathSet
getReferencesIncludeSelf cache path = do
  getReferencesFromCache cache path >>= \case
    Just refs -> pure refs
    Nothing -> do
      refs <- getRefs cache path
      addPath cache path
      recordReferences cache path refs
      pure refs

getReferences :: NixReferenceCache -> StorePath -> IO PathSet
getReferences cache path = do
  allrefs <- getReferencesIncludeSelf cache path
  pure $ HS.delete path allrefs

-- | Get a store path's references from the cache. Return Nothing if
-- the path isn't recorded in the cache yet. Caches in memory.
getReferencesFromCache
  :: NixReferenceCache -> StorePath -> IO (Maybe PathSet)
getReferencesFromCache cache path = do
  modifyMVar (nprcPathReferences cache) $ \tree -> do
    case H.lookup path tree of
      Just refs -> pure (tree, Just refs)
      Nothing -> getPathId cache path >>= \case
        Nothing -> pure (tree, Nothing)
        Just pathId -> do
          conn <- readMVar (nprcConnection cache)
          let qry = "select refs from Paths where id = ?"
          query conn qry (Only pathId) >>= \case
            -- If references have been recorded, parse and return them
            [Only (Just refs)] -> do
              let refTexts = T.words refs
              refs <- map HS.fromList $ forM refTexts $ \txt -> do
                case parseStorePath txt of
                  Right path -> pure path
                  Left err -> error $ "When parsing references of path "
                                   <> spToPath path <> ": " <> err
              pure (H.insert path refs tree, Just refs)
            _ -> pure (tree, Nothing)

-- | Get a store path's ID. Caches in memory.
getPathId :: NixReferenceCache -> StorePath -> IO (Maybe Int64)
getPathId cache path = do
  modifyMVar (nprcPathIdCache cache) $ \pathIdCache -> do
    case H.lookup path pathIdCache of
      Just pathId -> pure (pathIdCache, Just pathId)
      Nothing -> do
        let row = (Only $ spToText path)
        conn <- readMVar (nprcConnection cache)
        query conn "select id from Paths where path = ?" row >>= \case
          [Only pathId'] -> do
            pure (H.insert path pathId' pathIdCache, Just pathId')
          _ -> do
            pure (pathIdCache, Nothing)

-- | Store the references of a path. Caches in memory and in the DB.
recordReferences :: NixReferenceCache -> StorePath -> PathSet -> IO ()
recordReferences cache path refs = getPathId cache path >>= \case
  Nothing -> error "Can't add references, path hasn't been stored"
  Just pathId -> do
    modifyMVar_ (nprcPathReferences cache) $ \tree -> do
      case H.lookup path tree of
        Just refs' | refs == refs' -> pure tree
        Just _ -> error $ "Inconsistent reference lists for path " <> show path
        Nothing -> do
          -- First insert them into the database, then add to the cache
          withMVar (nprcConnection cache) $ \conn -> do
            let refsText = intercalate " " $ map spToText $ HS.toList refs
                qry = "update Paths set refs = ? where id = ?"
            execute conn qry (refsText, pathId)
            pure $ H.insert path refs tree


-- | Add a store path (if it's not there yet) and get its ID. There are
-- basically three cases here:
--
-- 1. In the in-memory cache: just return it.
-- 2. In the database: add the ID to the in-memory cache, return it.
-- 3. Not in the database: add it to the database, add new ID to the
--    in-memory cache, and return it.
addPath :: NixReferenceCache -> StorePath -> IO Int64
addPath cache path = do
  modifyMVar (nprcPathIdCache cache) $ \pathIdCache -> do
    case H.lookup path pathIdCache of
      Just pathId -> pure (pathIdCache, pathId)
      Nothing -> do
        let row = (Only $ spToText path)
        pathId <- withMVar (nprcConnection cache) $ \conn -> do
          query conn "select id from Paths where path = ?" row >>= \case
            [Only pathId'] -> pure pathId'
            _ -> do
              let row = Only (spToText path)
              execute conn "insert into Paths (path) values (?)" row
              lastInsertRowId conn
        pure (H.insert path pathId pathIdCache, pathId)


-- | Create the tables in the path cache
initializeReferenceCache :: NixReferenceCache -> IO ()
initializeReferenceCache nprc = do
  withMVar (nprcConnection nprc) $ \conn -> do
    execute_ conn $ fromString $
      "create table if not exists Paths " <>
      "(id integer primary key, path text unique not null, refs text)"
