{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module UseHaskellAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           UseHaskellAPI


restAPI :: Proxy API
restAPI = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

loadEnvVars     :: Maybe String      -> ClientM ResponseData
download        :: Instruction       -> ClientM FileData
update          :: Instruction_U     -> ClientM Bool
storeMessage    :: Message           -> ClientM Bool
searchMessage   :: Maybe String      -> ClientM [Message]
performRestCall :: Maybe String      -> ClientM ResponseData
list            :: Ticket            -> ClientM FileHere
init            :: ClientM Init
-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

(loadEnvVars :<|> download :<|> update :<|> storeMessage :<|> searchMessage :<|> performRestCall :<|> list :<|> init) = client restAPI
