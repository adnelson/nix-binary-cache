-- | Cache store paths and their references
module Nix.ReferenceCache (
  ReferenceCache(..),
  newReferenceCache, initializeReferenceCache,
  getReferences, getReferencesIncludeSelf, computeClosure,
  addPath, recordReferences
  ) where

import Database.SQLite.Simple (Connection, Query, Only(..), lastInsertRowId)
import Database.SQLite.Simple (open, execute_, execute, query, query_)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Text as T
import System.FilePath (takeDirectory)
import System.Environment (lookupEnv)

import Nix.Cache.Common hiding (log)
import Nix.Bin (NixBinDir(..), getNixBinDir, nixCmd)
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
data ReferenceCache = ReferenceCache {
  -- | Location of the nix store.
  nprcStoreDir :: NixStoreDir,
  -- | Connection to the local nix database (optional).
  nprcLocalNixDbConnection :: Maybe Connection,
  -- | Location on disk for nix binaries.
  nprcBinDir :: NixBinDir,
  -- | Location of the cache SQLite database.
  nprcCacheLocation :: FilePath,
  -- | Database connection for the local cache. Syncronized in MVar to
  -- allow lastrowid to be deterministic.
  nprcConnection :: MVar Connection,
  -- | Map store paths to their database row ID, so that we don't have to
  -- look them up all the time.
  nprcPathIdCache :: MVar (HashMap StorePath Int64),
  -- | Computed store path dependency tree.
  nprcPathReferences :: MVar PathTree,
  -- | Logging function.
  nprcLogger :: Maybe (Text -> IO ())
  }

log :: ReferenceCache -> Text -> IO ()
log cache msg = case nprcLogger cache of
  Nothing -> pure ()
  Just logger -> logger msg

-- | Attempt to connect to the local SQLite nix database. If the
-- connection fails, or it doesn't have a ValidPaths table, return Nothing.
attemptLocalNixConnection :: FilePath -> IO (Maybe Connection)
attemptLocalNixConnection dbpath = do
  putStrLn $ "Attempting local nix DB connection on DB path " <> tshow dbpath
  conn <- open dbpath
  let test = query_ conn "select count(*) from ValidPaths" :: IO [Only Int]
  (Just conn <$ test) `catch` \(err::SomeException) -> do
    putStrLn $ "Can't use local nix SQLite database: " <> tshow err
    pure Nothing

newReferenceCache :: IO ReferenceCache
newReferenceCache = do
  nprcCacheLocation <- getPathCacheLocation
  nprcLocalNixDbConnection <- attemptLocalNixConnection =<< getNixDBDir
  nprcStoreDir <- getNixStoreDir
  nprcBinDir <- getNixBinDir
  nprcConnection <- newMVar =<< open nprcCacheLocation
  nprcPathIdCache <- newMVar mempty
  nprcPathReferences <- newMVar mempty
  let nprcLogger = pure putStrLn
  pure ReferenceCache {..}

-- | Get the references of an object by asking either a nix command or the DB.
-- This information is cached by the caller of this function.
getRefs :: ReferenceCache -> StorePath -> IO (Maybe PathSet)
getRefs cache spath = case nprcLocalNixDbConnection cache of
  -- We can't access the DB directly. Use the CLI.
  Nothing -> getRefsFromNixCommand cache spath
  -- Pull the references directly out of the database.
  Just conn -> do
    -- Ensure it's in the nix DB by getting its ID
    let qry = "select id from ValidPaths where path = ?"
    let row = (Only $ spToFull (nprcStoreDir cache) spath)
    log cache $ "Running query " <> tshow qry <> " with path " <> tshow row
    query conn qry row >>= \case
      [Only (nixPathId :: Int64)] -> do
        refs <- query conn getPathsQuery (Only nixPathId)
        map (Just . HS.fromList) $
          map snd <$> mapM ioParseFullStorePath (map fromOnly refs)
      _ -> pure Nothing


-- | Get references of a path by querying the nix CLI.
getRefsFromNixCommand :: ReferenceCache -> StorePath -> IO (Maybe PathSet)
getRefsFromNixCommand cache spath = do
  let storeDir = nprcStoreDir cache
      args = ["--query", "--references", spToFull storeDir spath]
  map (Just . HS.fromList) (nixCmd (nprcBinDir cache) "store" args "")
    `catch` \(_::SomeException) -> pure Nothing

-- | Get the full runtime path dependency closure of a store path.
computeClosure :: ReferenceCache -> StorePath -> IO [StorePath]
computeClosure cache path = do
  let storeDir = nprcStoreDir cache
  nixCmd (nprcBinDir cache) "store" ["-qR", spToFull storeDir path] ""

-- | Query which will return all of the references of a path.
getPathsQuery :: Query
getPathsQuery = fromString $ concat [
  "select path from ValidPaths inner join (",
  "select reference from ValidPaths inner join Refs on id = referrer where id = ?",
  ") on id = reference"
  ]

-- | Get the references of a path, checking and updating the cache.
-- Doesn't filter out self-references.
getReferencesIncludeSelf :: ReferenceCache -> StorePath -> IO (Maybe PathSet)
getReferencesIncludeSelf cache path = do
  getReferencesFromCache cache path >>= \case
    Just refs -> pure $ Just refs
    Nothing -> do
      log cache $ "no cached reference set for " <> tshow path
      getRefs cache path >>= \case
        Nothing -> pure Nothing
        Just refs -> do
          addPath cache path
          recordReferences cache path refs
          pure $ Just refs

-- | Get a store path's references, excluding self-references.
getReferences :: ReferenceCache -> StorePath -> IO (Maybe PathSet)
getReferences cache path = do
  map (HS.delete path) <$> getReferencesIncludeSelf cache path

-- | Get a store path's references from the cache. Return Nothing if
-- the path isn't recorded in the cache yet. Caches in memory.
getReferencesFromCache
  :: ReferenceCache -> StorePath -> IO (Maybe PathSet)
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
getPathId :: ReferenceCache -> StorePath -> IO (Maybe Int64)
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
recordReferences :: ReferenceCache -> StorePath -> PathSet -> IO ()
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
addPath :: ReferenceCache -> StorePath -> IO Int64
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
initializeReferenceCache :: ReferenceCache -> IO ()
initializeReferenceCache nprc = do
  withMVar (nprcConnection nprc) $ \conn -> do
    execute_ conn $ fromString $
      "create table if not exists Paths " <>
      "(id integer primary key, path text unique not null, refs text)"
