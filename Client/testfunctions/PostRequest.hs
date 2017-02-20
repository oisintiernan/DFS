{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Servant.API
import           Servant.Client-- trying to make a post request with servant!!!!
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson                   
import           Data.Aeson.TH
import           Data.Bits
import           Data.Char
import qualified Data.ByteString.Lazy         as B
import qualified Data.ByteString.Lazy.Char8   as L
import qualified Data.List                    as DL
import qualified Data.List.Split              as DLS
import           Data.Maybe                   (catMaybes)
import           Control.Monad
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager,
                                               httpLbs,
                                               parseRequest,
                                               responseBody)
import           Network.Wai.Logger
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.IO
import           System.Directory
import           Data.Proxy
import           System.Posix.Files


authserverhost = "localhost"::String
authserverport = 8080       ::Int

data SignIn  =  SignIn { userName :: String
                       , passUser :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)

data AuthRes = AuthRes { sessionKey :: String
                       , tgt_enc    :: String
                       } deriving (Show, Generic, FromJSON, ToJSON) 

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON, Show)

data FileData = FileData { filep    ::FilePath
                         , contents ::String
                         , project  :: String
                         , filen    :: String
                         } deriving (Generic,ToJSON,FromJSON,Show)


data FileHere = FileHere { files  :: [FilePath]
                         } deriving (Generic,ToJSON,FromJSON,Show)                        
 
data Init = Init { purpose    :: String
                  ,functions  :: String
                  ,security   :: String
                 } deriving (Generic,ToJSON,FromJSON,Show)

data Message = Message { name     :: String
                       , message  :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)

data TGT = TGT         { tgt      :: String
                       , username :: String
                       , sKey     :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)    

data Ticket  =  Ticket { usern    :: String
                       , expTime  :: String
                       , seshKey  :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)

type API = "load_environment_variables" :> QueryParam "name" String :> Get '[JSON] ResponseData
      :<|> "download"                  :> QueryParam "path" FilePath :> Get '[JSON] FileData
      :<|> "update"                     :> ReqBody '[JSON] FileData  :> Post '[JSON] Bool
      :<|> "storeMessage"               :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
      :<|> "searchMessage"              :> QueryParam "name" String :> Get '[JSON] [Message]
      :<|> "performRESTCall"            :> QueryParam "filter" String  :> Get '[JSON] ResponseData
      :<|> "files"                      :> Get '[JSON] FileHere
      :<|> "init"                       :> Get '[JSON] Init
      :<|> "authInit"                   :> ReqBody '[JSON] SignIn :> Post '[JSON] AuthRes
      :<|> "ticketGrantingService"      :> ReqBody '[JSON] TGT  :> Post '[JSON] Ticket

restAPI :: Proxy API
restAPI = Proxy

loadEnvVars     :: Maybe String       -> ClientM ResponseData
download        :: Maybe FilePath     -> ClientM FileData
update          :: FileData          -> ClientM Bool
storeMessage    :: Message            -> ClientM Bool
searchMessage   :: Maybe String       -> ClientM [Message]
performRestCall :: Maybe String       -> ClientM ResponseData
file            :: ClientM FileHere
init            :: ClientM Init
authInit        :: SignIn             -> ClientM AuthRes
ticketGrantingService :: TGT          -> ClientM Ticket



loadEnvVars :<|> download :<|> update:<|> storeMessage :<|> searchMessage :<|> performRestCall :<|> file :<|> init  :<|> authInit :<|> ticketGrantingService = client restAPI

encryptDecrypt :: String -> String -> String
encryptDecrypt key text = zipWith (\a b -> chr $ xor (ord a) (ord b)) (cycle key) text

main :: IO()
main = do
manager <- newManager defaultManagerSettings
res <- runClientM (authInit (SignIn "oisin" "][A[\\")) (ClientEnv manager (BaseUrl Http authserverhost (8000) ""))
case res of
  Left err -> do
    print "ran into some errors der"
  Right (authInit) -> do
    let (AuthRes enc_sk enc_tgt) = authInit
    print $ enc_tgt ++ enc_sk ++ "    auth response"
    let tgt = encryptDecrypt "222" enc_tgt--correct
    print $ tgt ++ "    tgt"
    let sk = encryptDecrypt "22" enc_sk
    let tgt_un = encryptDecrypt enc_tgt "oisin"
    let tgt_sk = encryptDecrypt enc_tgt sk
    a <- getLine
    res1 <- runClientM (ticketGrantingService (TGT enc_tgt tgt_un tgt_sk)) (ClientEnv manager (BaseUrl Http authserverhost (8000) ""))
    print res1
    case res1 of
      Left err -> do
        print "error alert"
      Right (ticketGrantingService) -> do
        let (Ticket u t k) = ticketGrantingService
        print u 
