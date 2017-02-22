{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module UseHaskellAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant




data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)


deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

-- | We will also define a simple data type for returning data from a REST call, again with nothing special or
-- particular in the response, but instead merely as a demonstration.

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)

data FileData = FileData { contents::String
                         , filen   :: String
                         } deriving (Generic,ToJSON,FromJSON,FromBSON,Show)


data FileHere = FileHere { files:: [FilePath]
                         } deriving (Generic,ToJSON,FromJSON,Show)                        

data Init = Init { purpose:: String
                  ,functions:: String
                  ,security:: String
                 } deriving (Generic,ToJSON,FromJSON,FromBSON,Show)

data Ticket  =  Ticket { usern    :: String
                       , expTime  :: String
                       , seshKey  :: String
                       } deriving (Show, Generic, FromJSON, ToJSON)

data Instruction = Instruction { command :: String
                               , ticket  :: Ticket
                               } deriving (Show, Generic, FromJSON, ToJSON)

data Instruction_U = Instruction_U { file    :: FileData
                                   ,tick  :: Ticket
                                 } deriving (Show, Generic, FromJSON, ToJSON)



type API = "load_environment_variables" :> QueryParam "name" String        :> Get  '[JSON] ResponseData
      :<|> "download"                   :> ReqBody '[JSON] Instruction     :> Post '[JSON] FileData
      :<|> "update"                     :> ReqBody '[JSON] Instruction_U   :> Post '[JSON] Bool
      :<|> "storeMessage"               :> ReqBody '[JSON] Message         :> Post '[JSON] Bool
      :<|> "searchMessage"              :> QueryParam "name" String        :> Get  '[JSON] [Message]
      :<|> "performRESTCall"            :> QueryParam "filter" String      :> Get  '[JSON] ResponseData
      :<|> "list"                       :> ReqBody '[JSON] Ticket          :> Post '[JSON] FileHere
      :<|> "init"                       :> Get '[JSON] Init
