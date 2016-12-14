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
import qualified Data.List.Split              as DLS
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
import           System.Directory
import           System.Posix.Files


data FileData = FileData { filep::FilePath
                         , contents::String
                         , project:: String
                         , filen:: String
                         } deriving (Generic,A.ToJSON,A.FromJSON,Show)


data FileHere = FileHere { files:: [FilePath]
                         } deriving (Generic,A.ToJSON,A.FromJSON,Show)                        

data Init = Init { purpose:: String
                  ,functions:: String
                  ,security:: String
                 } deriving (Generic,A.ToJSON,A.FromJSON,Show)



--FunctionS
getF       = "http://localhost:8080/getREADME?path=" :: String
viewF      = "http://localhost:8080/files?path="     :: String
initialise = "http://localhost:8080/init"            :: String
--Always
pathL      = "/home/ois/DFS/use-haskell/src/TF/"     :: String
cache      = "/home/ois/Client/cache/"               :: FilePath
cP         = "/home/ois/Client/CurrentProjects/"     :: String
--Query Params


main::IO()
main = do
  putStrLn "File Server Initialised.... Sending Initial Command"
--Initialise tells the user the various functions they have available
  a <- getAndParseI $ getJSON $ initialise
  putStrLn  $ (purpos a) ++ (func a) ++ (sec a)
--control is a looping function, it reads user input and then directs the program toward appropriate functionality
  control

control::IO()
control = do
  putStrLn "Enter Function"
  a <- getLine
  case a of
    "view" -> viewProjects >> control
    "read" -> readFiles >> control
    _      -> putStrLn "Invalid function. Try again" >> control
  --case a of
    --"Y" -> viewProjects
    --_ -> readFiles


--Formatting initial String
fInit :: String -> String -> String -> IO()
fInit purp funct secu = do
  putStrLn $ purp ++ funct ++ secu

--Functions available to the user

viewProjects::IO()
viewProjects = do
  d <-getAndParseL $ getJSON $ viewF ++ pathL
  putStrLn "View a certain project?(Y/N)"
  a <- getLine
  case a of
    "Y" -> viewFiles
    "N" -> putStrLn "back to main menu" >> control
    _   -> putStrLn "error:invalid request" >> viewProjects


viewFiles::IO()
viewFiles = do
  putStrLn "What project are you interested in?"
  proj <- getLine
  putStrLn "What file are you interested in?"
  file <- getLine
  d <-getAndParseL $ getJSON $ viewF ++ pathL ++ proj ++ "/" ++ file
  putStrLn "viewFiles"


readFiles::IO()
readFiles = do
  putStrLn "Which Project?"
  proj <- getLine
  putStrLn "Which file?"
  file <- getLine
  status <- getDirectoryContents cache
  case (elem (proj ++ "-" ++ file) (status)) of
    True   -> getAndWriteCD  proj file
    False  -> getAndWrite    proj file
  --WRITING TO CACHE
  --writeFile (cache ++ (projec d) ++  "-" ++ (filename d)) (content d)
  --WRITING TO CURRENT PROJECTS
  --writeFile (cP ++ (projec d) ++  "-" ++ (filename d)) (content d)
  --putStrLn "file saved to CurrentProject and cached in cache directory"

getAndWrite:: String -> String -> IO()
getAndWrite proj file = do
  d <- getAndParseF $ getJSON $  getF ++ pathL ++ proj ++ "/" ++ file
--WRITING TO CACHE
  writeFile (cache ++ (projec d) ++  "-" ++ (filename d)) (content d)
--WRITING TO CURRENT PROJECTS
  writeFile (cP ++ (projec d) ++  "-" ++ (filename d)) (content d)
  putStrLn $ "writing file: " ++ proj ++ "-" ++ file ++ " to both cache and currentProjects"

getAndWriteCD:: String -> String -> IO()
getAndWriteCD proj file = do
  a <- readFile $ cache ++ proj ++ "-" ++ file
--WRITING TO CURRENT PROJECTS
  writeFile (cP ++ proj ++  "-" ++ file) a
  putStrLn $ "writing file: " ++ proj ++  "-" ++ file ++ " to current directory"



  --WRITING TO CURRENT PROJECTS


--rFiles:: String -> String ->  Maybe FileData -> IO()
--rFiles proj file = do
  --d <- getAndParseF $ getJSON $  getF ++ pathL ++ proj ++ "/" ++ file
  --putStrLn "hello"
  --writeFile (cache ++ (projec d) ++  "-" ++ (filename d)) (content d)
  --writeFile (cP ++ (projec d) ++  "-" ++ (filename d)) (content d)

--Send Get request and parse back to haskell data type

getJSON :: String -> IO B.ByteString
getJSON string = do
  simpleHttp string

getAndParseI :: IO L.ByteString -> IO(Maybe Init)
getAndParseI url = do
  (A.decode <$> url) :: IO (Maybe Init)

getAndParseL :: IO L.ByteString -> IO(Maybe FileHere)
getAndParseL url = do
  (A.decode <$> url) :: IO (Maybe FileHere)
  
getAndParseF :: IO L.ByteString -> IO (Maybe FileData)
getAndParseF url = do
  (A.decode <$> url) :: IO (Maybe FileData)


--Decodde Haskell File types
--TEXT FILES
content :: Maybe FileData -> String
content (Just (FileData _  contents _ _)) = contents

filepath :: Maybe FileData -> FilePath
filepath (Just (FileData filep  _ _ _)) = filep

projec :: Maybe FileData -> String
projec (Just (FileData _ _ project _)) = project

filename :: Maybe FileData -> String
filename (Just (FileData _ _ _ filen)) = filen

--VIEWING FILES
location :: Maybe FileHere -> [FilePath]
location (Just (FileHere [files])) = [files]

--INITIALISE
purpos :: Maybe Init -> String
purpos (Just (Init purpose _ _)) = purpose

func :: Maybe Init -> String
func (Just (Init _ functions _)) = functions

sec :: Maybe Init -> String
sec (Just (Init _ _ security)) = security


--String Manipulation
--Split up file paths
splitFP :: String -> String
splitFP fp = do
  (last (DLS.splitOn "/" fp))

--Take quotation marks away when outputting list slot
sq :: String -> String
sq s@[c]                     = s
sq s        | last s == '"'  = init s
        | otherwise          = s











