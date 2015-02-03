module Main where

{- DEPENDENCIES
   hmatrix      (cabal install hmatrix)
   http-conduit (cabal install http-conduit)
   zip-archive  (cabal install zip-archive)
-}

import qualified Params.Post  as P
import qualified Utils.File   as F
import qualified Utils.Result as R
import qualified Utils.Web    as W

main :: IO ()
main = do
       requests  <- P.mkReqList 
       responses <- W.getFiles       requests           []
       paths     <- W.saveFiles      P.opts   responses []
       outputs   <- F.makeWritePaths paths              []
       R.runAnalyses paths outputs
       putStrLn "Analysis Complete"
