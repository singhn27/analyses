module Utils.Matrix where

{- Modules -}

import Data.Array
import Data.Graph
import Data.List
import Numeric.LinearAlgebra

{- Functions -}

-- Matrix Derivation --

deriveDMatrix :: Graph -> Int -> IO (Matrix Double)
deriveDMatrix g n = return mkDegMx
    where mkDegMx = diagRect 0 (fromList lstDegs) n n
          lstDegs = map getDegs $ assocs $ outdegree g
          getDegs = fromIntegral . snd

deriveAMatrix :: Graph -> Int -> IO (Matrix Double)
deriveAMatrix g n = return mkAdjMx
    where mkAdjMx = (n><n) (map fromIntegral convAdj)
          convAdj = map (\(a,b) -> if (a `elem` indices) then (\x -> 1) b else b) matrix0
          matrix0 = zip [1..] $ concat square0
          square0 = [(replicate n 0) | x <- (replicate n 0)]
          indices = sort $ concat $ map getInds (assocs g)
          getInds = (\(a,b) -> if (length b > 0) then (map (((a - 1) * n) +)) b else b)

-- Laplacian Computation --

computeL :: Graph -> Int -> IO (Matrix Double)
computeL g n = 
    do d <- deriveDMatrix g n
       a <- deriveAMatrix g n
       return $ d + (-1) * a

-- Eigenvalue Computation --

computeEigs :: Matrix Double -> IO [Double]
computeEigs l = return $ toList $ sqrt . eigenvaluesSH $ l <> trans l 
