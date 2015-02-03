module Utils.Graph where

{- Modules -}

import Data.Graph
import Data.List
import Data.Maybe

{- Functions -}

-- Data Cleanup --

pruneCodes :: [[String]] -> IO [(String, String)]
pruneCodes f = return $ nub $ mkTup $ rmEmp $ drop 1 f
 where mkTup = map (\[a,b] -> (a,b))
       rmEmp = map (filter (not . null))

constructEdges :: [(String, String)] -> IO ([(String, Int)], [(Int, Int)])
constructEdges p = return (verts, edges)
     where verts = flip zip [1..] $ sort $ nub $ listE
           listE = foldr (\(a,b) x -> a : b : x) [] p
           edges = map (\(a,b) -> (fromJust $ lookup a verts, fromJust $ lookup b verts)) p

constructEdgeList :: [[String]] -> IO ([(String, Int)], [(Int, Int)])
constructEdgeList f = 
    do pruned <- pruneCodes f
       (v, e) <- constructEdges pruned
       return (v, e)

-- Graph Construction --

constructGraph :: [[String]] -> IO (Graph, (Int, [(String, Int)]))
constructGraph f = 
    do (v, e) <- constructEdgeList f
       return ((buildG (1, length v) e), (length e, v))
