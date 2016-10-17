module Nix.Cache.Client where

import ClassyPrelude
import Data.Aeson
import Data.Proxy
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Control.Monad.Trans.Except (ExceptT, runExceptT)

-- import Nix.Cache.Types

data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)

instance FromJSON Email

type API = "fart" :> Get '[JSON] Int
      :<|> "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

fart :: Manager -> BaseUrl -> ExceptT ServantError IO Int

position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> Manager -- ^ the HTTP client to use
         -> BaseUrl -- ^ the URL at which the API can be found
         -> ExceptT ServantError IO Position

hello :: Maybe String -- ^ an optional value for "name"
      -> Manager -- ^ the HTTP client to use
      -> BaseUrl -- ^ the URL at which the API can be found
      -> ExceptT ServantError IO HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> Manager -- ^ the HTTP client to use
          -> BaseUrl -- ^ the URL at which the API can be found
          -> ExceptT ServantError IO Email


fart :<|> position :<|> hello :<|> marketing = client (Proxy :: Proxy API)

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (Position, HelloMessage, Email)
queries manager baseurl = do
  pos <- position 10 10 manager baseurl
  message <- hello (Just "servant") manager baseurl
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]) manager baseurl
  return (pos, message, em)

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  res <- runExceptT (queries manager (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ tshow err
    Right (pos, message, em) -> do
      print pos
      print message
      print em
