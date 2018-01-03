-- | Cache store paths and their references
module Nix.ReferenceCache (
  ReferenceCache(..),
  newReferenceCache, initializeReferenceCache,
  getReferences, getReferencesIncludeSelf, computeClosure,
  addPath, recordReferences, getDeriver, recordDeriver,
  getSignature, recordSignature
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
import Nix.Nar.Types (Signature(..), KeyName(..))
import Nix.StorePath (NixStoreDir(..), PathTree, PathSet, StorePath, spToText)
import Nix.StorePath (getNixStoreDir, spToFull, parseStorePath, spToPath)
import Nix.StorePath (ioParseStorePath)

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
  -- | Computed store path derivers. Not all paths have known derivers.
  nprcPathDerivers :: MVar (HashMap StorePath (Maybe StorePath)),
  -- | Signatures. Each path can have up to one signature per public key.
  nprcPathSignatures :: MVar (HashMap StorePath (HashMap KeyName ByteString)),
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
  nprcPathDerivers <- newMVar mempty
  nprcPathSignatures <- newMVar mempty
  let nprcLogger = pure putStrLn
  pure ReferenceCache {..}

-- | Get the references of an object by asking either a nix command or the DB.
-- This information is cached by the caller of this function.
getReferencesUncached :: ReferenceCache -> StorePath -> IO (Maybe PathSet)
getReferencesUncached cache spath = case nprcLocalNixDbConnection cache of
  -- We can't access the DB directly. Use the CLI.
  Nothing -> do
    let storeDir = nprcStoreDir cache
        args = ["--query", "--references", spToFull storeDir spath]
    map (Just . HS.fromList) (nixCmd (nprcBinDir cache) "store" args "")
      `catch` \(_::SomeException) -> pure Nothing
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
          mapM ioParseStorePath (map fromOnly refs)
      _ -> pure Nothing

-- | Get a store path's deriver from the cache, and update the cache.
--
-- Note the return type here:
-- * If the store path isn't in the database, we'll return Nothing
-- * If it's in the database, but has no deriver, we'll return (Just Nothing)
-- * Otherwise, we'll return (Just (Just <deriver path>))
getDeriver :: ReferenceCache -> StorePath -> IO (Maybe (Maybe StorePath))
getDeriver cache spath = do
  let
    getUncached = case nprcLocalNixDbConnection cache of
      -- We can't access the DB directly. Use the CLI.
      Nothing -> do
        let storeDir = nprcStoreDir cache
            args = ["--query", "--deriver", spToFull storeDir spath]
            cmd = nixCmd (nprcBinDir cache) "store" args "" >>= \case
              "unknown-deriver" -> pure (Just Nothing)
              path -> Just . Just <$> ioParseStorePath path
        catch cmd (\(_::SomeException) -> pure Nothing)
      -- Pull the deriver directly out of the database.
      Just conn -> do
        -- Ensure it's in the nix DB by getting its ID
        let qry = "select id, deriver from ValidPaths where path = ?"
        let row = (Only $ spToFull (nprcStoreDir cache) spath)
        log cache $ "Running query " <> tshow qry <> " with path " <> tshow row
        query conn qry row >>= \case
          [Only (Just deriverTxt)] -> Just . Just <$> ioParseStorePath deriverTxt
          [Only Nothing] -> pure (Just Nothing)
          _ -> pure Nothing

    getFromCache = do
      modifyMVar (nprcPathDerivers cache) $ \derivers -> do
        case H.lookup spath derivers of
          Just maybeDeriver -> pure (derivers, Just maybeDeriver)
          Nothing -> getPathId cache spath >>= \case
            Nothing -> pure (derivers, Nothing)
            Just pathId -> do
              conn <- readMVar (nprcConnection cache)
              let qry = "select deriver from Paths where id = ?"
              query conn qry (Only pathId) >>= \case
                -- If references have been recorded, parse and return them
                [Only (Just deriverText)] -> do
                  mDeriver <- case deriverText of
                                "unknown-deriver" -> pure Nothing
                                path -> Just <$> ioParseStorePath path
                  pure (H.insert spath mDeriver derivers, Just mDeriver)
                _ -> pure (derivers, Nothing)

  getFromCache >>= \case
    Just result -> pure $ Just result
    Nothing -> do
      log cache $ "no cached deriver for " <> tshow spath
      getUncached >>= \case
        Nothing -> pure Nothing
        Just maybeDeriver -> do
          addPath cache spath
          recordDeriver cache spath maybeDeriver
          pure $ Just maybeDeriver

-- | Get the full runtime path dependency closure of a store path.
computeClosure :: ReferenceCache -> StorePath -> IO PathSet
computeClosure cache path = HS.fromList <$> do
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
getReferencesIncludeSelf cache spath = do
  let
    getCached = do
      modifyMVar (nprcPathReferences cache) $ \tree -> do
        case H.lookup spath tree of
          Just refs -> pure (tree, Just refs)
          Nothing -> getPathId cache spath >>= \case
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
                                       <> spToPath spath <> ": " <> err
                  pure (H.insert spath refs tree, Just refs)
                _ -> pure (tree, Nothing)

  getCached >>= \case
    Just refs -> pure $ Just refs
    Nothing -> do
      log cache $ "no cached reference set for " <> tshow spath
      getReferencesUncached cache spath >>= \case
        Nothing -> pure Nothing
        Just refs -> do
          recordReferences cache spath refs
          pure $ Just refs

-- | Get a store path's references, excluding self-references.
getReferences :: ReferenceCache -> StorePath -> IO (Maybe PathSet)
getReferences cache path = do
  map (HS.delete path) <$> getReferencesIncludeSelf cache path

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
recordReferences cache path refs = do
  pathId <- addPath cache path
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

-- | Record a path's deriver.
recordDeriver :: ReferenceCache -> StorePath -> Maybe StorePath -> IO ()
recordDeriver cache path mDeriver = do
  pathId <- addPath cache path
  modifyMVar_ (nprcPathDerivers cache) $ \derivers -> do
    let storeDir = nprcStoreDir cache
        mDeriverTxt = case mDeriver of
          Just dpath -> spToFull storeDir dpath
          Nothing -> "unknown-deriver"
        qry = "update Paths set deriver = ? where id = ?"
        add = withMVar (nprcConnection cache) $ \conn -> do
          execute conn qry (mDeriverTxt, pathId)
          pure $ H.insert path mDeriver derivers
    case H.lookup path derivers of
      -- Case 1: we don't have a deriver path (or lack thereof)
      -- recorded, so update the database and cache
      Nothing -> add
      -- Case 2: we have recorded that there's no deriver, but now
      -- we *do* have a deriver. Update the cache.
      Just Nothing | isJust mDeriver -> add
      -- Case 3: we already have a deriver path (or lack thereof)
      -- recorded, and this doesn't add any new information. Leave
      -- things unchanged.
      _ -> pure derivers

getSignature :: ReferenceCache -> StorePath -> KeyName -> IO (Maybe ByteString)
getSignature cache spath key@(KeyName name) = do
  pathId <- addPath cache spath
  let
    readFromDB = do
      conn <- readMVar (nprcConnection cache)
      let sql = fromString $
            "select signature from Signatures inner join Paths " <>
            "on path_id = id where id = ? and key_name = ?"
      query conn (fromString sql) (pathId, name) >>= \case
        [Only signature] -> pure $ Just signature
        _ -> pure Nothing
  modifyMVar (nprcPathSignatures cache) $ \sigs -> do
    case H.lookup spath sigs of
      Just sigsByKey -> case H.lookup key sigsByKey of
        -- in the outer map and inner map
        Just sigBytes -> pure (sigs, Just sigBytes)
        -- in the outer map but not inner map
        Nothing -> readFromDB >>= \case
          -- not in the database: give up
          Nothing -> pure (sigs, Nothing)
          -- in the database
          Just sigBytes -> do
            let sigsByKey' = H.insert key sigBytes sigsByKey
            pure (H.insert spath sigsByKey' sigs, Just sigBytes)
      -- not in the outer map
      Nothing -> readFromDB >>= \case
        Nothing -> pure (sigs, Nothing)
        Just sigBytes -> do
          pure (H.insert spath (H.singleton key sigBytes) sigs, Just sigBytes)


recordSignature :: ReferenceCache -> StorePath -> Signature -> IO ()
recordSignature cache spath (Signature key sig) = do
  pathId <- addPath cache spath
  let
    recordInDB = withMVar (nprcConnection cache) $ \conn -> do
      let KeyName keyName = key
      let qry = fromString $
            "insert or ignore into Signatures " <>
            "(path_id, key_name, signature) values (?, ?, ?)"
      execute conn qry (pathId, keyName, sig)

  modifyMVar_ (nprcPathSignatures cache) $ \signatures -> do
    case H.lookup spath signatures of
      Just sigsByKey -> case H.lookup key sigsByKey of
        Just sig' | sig == sig' -> pure signatures
        Just sig' | otherwise -> error $ concat [
          "Conflicting signatures for ", show spath, ": ", show sig',
          " is stored, but trying to store new signature ", show sig
          ]
        Nothing -> do
          recordInDB
          pure (H.insert spath (H.insert key sig sigsByKey) signatures)
      Nothing -> do
        recordInDB
        pure (H.insert spath (H.singleton key sig) signatures)

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
      "(id integer primary key, path text unique not null, " <>
      "refs text, deriver text)"

    execute_ conn $ fromString $
      "create table if not exists Signatures " <>
      "(path_id integer not null references Paths(id) on delete cascade, " <>
      "key_name text not null, signature text not null, " <>
      "constraint one_sig_per_key_per_path unique " <>
      "(path_id, key_name, signature))"
