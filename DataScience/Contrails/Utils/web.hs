{-# LANGUAGE OverloadedStrings #-}

module Utils.Web where

{- Modules -}

import Codec.Archive.Zip
import Control.Monad.Trans.Resource (runResourceT)
import Data.Maybe
import Network
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.Directory

import qualified Data.ByteString.Lazy.Char8 as C

{- Parameters -}

url :: String
url =  "http://www.transtats.bts.gov/DownLoad_Table.asp?Table_ID=236&Has_Group=3&Is_Zipped=0"

{- Functions -}

-- Main --

getFiles :: [String] -> [Response C.ByteString] -> IO [Response C.ByteString]
getFiles (r:rs) results = do
    response <- runResourceT $ do
        initReq <- parseUrl url
        let request = initReq { method = "POST"
                              , redirectCount = 1
                              , requestBody = RequestBodyLBS (C.pack r)
                              , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                              }
        withManager $ httpLbs request
    getFiles rs (response:results)
getFiles [] results = return results

saveFiles :: [((String, Int), Int)] -> [Response C.ByteString] -> [FilePath] -> IO [FilePath]
saveFiles (n:ns) (b:bs) results = do
    let zip     = toArchive $ responseBody b
    let zipname = "data/" ++ (head $ filesInArchive zip)
    let newname = "data/" ++ ((show $ snd $ fst n) ++ "-" ++ (show $ snd n) ++ ".csv")
    extractFilesFromArchive [OptDestination "data/"] zip
    renameFile (zipname :: FilePath) (newname :: FilePath)
    let nx = (newname:results)
    saveFiles ns bs nx
saveFiles [] [] x = return $ reverse x
