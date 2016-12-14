-- This file is commented extensively for non-haskell programmers

-- | These are language extensions. Haskell has a great many language
-- extensions but in practice you do not need to knwo much about them. If you
-- use a library that needs them, then the library documentation will tell you which
-- extensions you neeed to include. If you try to write code that needs particular extensions,
-- then the haskell compiler is smart enough typically to be able to suggest which extensions
-- you should switch on by including an entry here.

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

-- | Haskell code is structured as sets of functions that sit within Modules. The basic rule is that a module with a
-- particular name (for example Lib) sits within a .hs file of the same name (eg. Lib.hs). The module statement is of
-- the form `module MODULE_NAME (EXPORTED_FUNCTIONS) where`. Everything following this is part of the module. There are
-- no brackets or any other syntax to worry about.
module Lib
    ( startApp
    ) where

-- | Imports work like most other languages and are essentially library includes. The functions of the lirbary become
-- immediately accessible in the code of the module. There are various ways in which imports can be modified. For
-- example, one may `import qualified X as Y` which imports a library in such a way that the functions of the library
-- must be prefixed with `Y.`. One can always prefix a libraries functions with the import string, when calling them.
-- You will occasionally have reason to import libraries that have common function names by coincidence. You can use
-- qualified imports of full prefixes to disambiguate. The compiler will tell you where the problem is if this occurs.

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import qualified Data.Aeson                   as A
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy         as B
import qualified Data.ByteString.Lazy.Char8   as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager,
                                               httpLbs,
                                               parseRequest,
                                               responseBody)
import           Network.HTTP.Conduit
import           Network.Wai.Logger
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.IO


data FileData = FileData { file::FilePath
                         , contents::String
                         } deriving (Generic,A.ToJSON,A.FromJSON,Show)



--FunctionS
getF = "http://localhost:8080/getREADME?path="
viewF = "http://localhost:8080/files?path="
initialise = "http://localhost:8080/init"
--Always
pathL = "/home/ois/DFS/use-haskell/src/TF/"
--Query Params


startApp::IO()
startApp = do
  print "File Server Initialised.... Sending Initial Command\n"
  getAndParse $ getJSON $ initialise
  control

control::IO()
control = do
  putStrLn "Enter Function\n"
  a <- getLine
  case a of
    "view" -> viewProjects
    "read" -> readFiles
    _      -> putStrLn "Invalid function. Try again" >> control
  --case a of
    --"Y" -> viewProjects
    --_ -> readFiles


viewProjects::IO()
viewProjects = do
    
    putStrLn "Search a project? (Y/N)"
    a <-getLine
    case a of
        "Y" -> viewFiles
        _ -> startApp


viewFiles::IO()
viewFiles = do
  putStrLn "What project are you interested in?"
  proj <- getLine
  --getR (files ++ fileL ++ proj) (view)
  putStrLn "Edit a file? (Y/N)"
  a <- getLine
  case a of
    "Y" -> readFiles
    _ -> startApp


readFiles::IO()
readFiles = do
  putStrLn "Which Project?"
  a <- getLine
  putStrLn "Which file?"
  b <- getLine
  let c = "file"
  putStrLn "test"
  getAndParse $ getJSON $ jsonURL

jsonURL :: String
jsonURL = "http://localhost:8080/getREADME?path=/home/ois/DFS/use-haskell/src/TF/Project1/Costs"


getJSON :: String -> IO B.ByteString
getJSON string = do
  simpleHttp string

getAndParse :: IO L.ByteString -> IO()
getAndParse url = do
  d <- (A.eitherDecode <$> url) :: IO (Either String FileData)
  case d of
    Left err -> putStrLn err
    Right ps -> print ps
