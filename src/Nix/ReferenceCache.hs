-- | Cache store paths and their references
module Main where

import Database.SQLite.Simple (FromRow(..), ToRow(..), Connection, Query)
import Database.SQLite.Simple (Only(..), field, lastInsertRowId)
import Database.SQLite.Simple (open, execute_, execute, query, query_)
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Text as T
import System.Process.Text (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Nix.Cache.Common
import Nix.Cache.Client hiding (writeCache, writeCacheFor)
import Nix.Bin
import Nix.StorePath -- (StorePath(..))
import Nix.NarInfo ()

data TestField = TestField Int Text deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

-- | Path reference cache, backed by sqlite
data NixPathReferenceCache = NixPathReferenceCache {
  nprcStoreDir :: NixStoreDir,
  nprcLocalNixDbConnection :: Maybe Connection,
  -- ^ What's the path to the nix DB?
  nprcBinDir :: NixBinDir,
  nprcCacheLocation :: FilePath,
  -- ^ Where does the cache live on disk?
  nprcConnection :: MVar Connection,
  -- ^ Database connection for the local cache. Syncronized in MVar to
  -- allow lastrowid to be deterministic.
  nprcPathIdCache :: MVar (HashMap StorePath Int64),
  -- ^ Map store paths to their database row ID, so that we don't have to
  -- look them up all the time.
  nprcPathTree :: MVar PathTree
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

newPathReferenceCache :: FilePath -> IO NixPathReferenceCache
newPathReferenceCache nprcCacheLocation = do
  nprcLocalNixDbConnection <- attemptLocalNixConnection "/nix/var/nix/db/db.sqlite"
  nprcStoreDir <- getNixStoreDir
  nprcBinDir <- getNixBinDir
  nprcConnection <- newMVar =<< open nprcCacheLocation
  nprcPathIdCache <- newMVar mempty
  nprcPathTree <- newMVar mempty
  pure NixPathReferenceCache {..}

-- | Get the references of an object by asking the nix-store. This
-- information is cached by the caller of this function.
getRefsFromNixDB :: NixPathReferenceCache -> StorePath -> IO PathSet
getRefsFromNixDB cache spath = case nprcLocalNixDbConnection cache of
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

getRefsFromNixCommand :: NixPathReferenceCache -> StorePath -> IO PathSet
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

-- writeCacheFor :: Connection -> StorePath -> [StorePath] -> NixClient ()
-- writeCacheFor conn path refs = do


-- writeCache :: NixClient ()
--   cacheLocation <- nccCacheLocation <$> ncoConfig <$> ask
--   conn <- liftIO $ open "test.db"
--   mv <- ncoState <$> ask
--   tree <- ncsPathTree <$> readMVar mv
--   forM_ (H.toList tree) $ \(path, refs) -> do
--     writeCacheFor path refs

-- | Query which will return all of the references of a path.
getPathsQuery :: Query
getPathsQuery = fromString $ concat [
  "select path from ValidPaths inner join (",
  "select reference from ValidPaths inner join Refs on id = referrer where id = ?",
  ") on id = reference"
  ]

getPathReferences :: NixPathReferenceCache -> StorePath -> IO PathSet
getPathReferences cache path = getPathReferencesFromCache cache path >>= \case
  Just refs -> do
    putStrLn $ tshow path <> " is in the cache"
    pure refs
  Nothing -> do
    putStrLn $ tshow path <> " not in cache, querying nix"
    refs <- getRefsFromNixDB cache path
    addPath cache path
    storePathReferences cache path refs
    pure refs

getpaths :: Int -> IO [StorePath]
getpaths n = do
  NixStoreDir d <- getNixStoreDir
  items <- take n <$> listDirectory d
  forM items $ \item -> do
    let Right sp = parseStorePath $ pack item
    pure sp

-- | Get a store path's references from the cache. Return Nothing if
-- the path isn't recorded in the cache yet. Caches in memory.
getPathReferencesFromCache
  :: NixPathReferenceCache -> StorePath -> IO (Maybe PathSet)
getPathReferencesFromCache cache path = do
  modifyMVar (nprcPathTree cache) $ \tree -> do
    case H.lookup path tree of
      Just refs -> pure (tree, Just refs)
      Nothing -> getPathId cache path >>= \case
        Nothing -> do
          putStrLn $ tshow path <> " not recorded"
          pure (tree, Nothing)
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
            _ -> do
              putStrLn $ tshow path <> " references not recorded"
              pure (tree, Nothing)

-- | Get a store path's ID. Caches in memory.
getPathId
  :: NixPathReferenceCache -> StorePath -> IO (Maybe Int64)
getPathId cache path = do
  modifyMVar (nprcPathIdCache cache) $ \pathIdCache -> do
    case H.lookup path pathIdCache of
      Just pathId -> pure (pathIdCache, Just pathId)
      Nothing -> do
        let row = (Only $ spToText path)
        withMVar (nprcConnection cache) $ \conn -> do
          query conn "select id from Paths where path = ?" row >>= \case
            [Only pathId'] -> do
              pure (H.insert path pathId' pathIdCache, Just pathId')
            _ -> do
              pure (pathIdCache, Nothing)

-- | Store the references of a path. Caches in memory and in the DB.
storePathReferences
  :: NixPathReferenceCache -> StorePath -> PathSet -> IO ()
storePathReferences cache path refs = getPathId cache path >>= \case
  Nothing -> error "Can't add references, path hasn't been stored"
  Just pathId -> do
    modifyMVar_ (nprcPathTree cache) $ \tree -> do
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
addPath :: NixPathReferenceCache -> StorePath -> IO Int64
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
initializePathCache :: NixPathReferenceCache -> IO ()
initializePathCache nprc = do
  withMVar (nprcConnection nprc) $ \conn -> do
    execute_ conn $ fromString $
      "create table if not exists Paths " <>
      "(id integer primary key, path text unique not null, refs text)"
    execute_ conn
      "create table if not exists Refs (referrer integer, reference integer)"

getCache :: IO NixPathReferenceCache
getCache = do
  newPathReferenceCache =<< map nccCacheLocation loadClientConfig

getConn :: IO Connection
getConn = do
  cache <- getCache
  readMVar $ nprcConnection cache

main :: IO ()
main = do
  cfg <- loadClientConfig
  cache <- newPathReferenceCache (nccCacheLocation cfg)
  initializePathCache cache
  {-
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
  rowId <- lastInsertRowId conn
  executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: Text), ":id" := rowId]
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
  close conn
  -}
