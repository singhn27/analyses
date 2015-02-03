module Utils.Result where

{- Modules -}

import Data.Graph
import Data.List
import Data.Maybe

import qualified Utils.File   as F
import qualified Utils.Graph  as G
import qualified Utils.Matrix as M

{- Functions -}

-- Numerical Manipulation -- 

rounded :: Double -> Int -> Double
rounded f n = (fromInteger $ round $ f * (10 ^ n)) / (10.0 ^^ n)

-- Connectedness Computation --

computeAlgCon :: [Double] -> IO Double
computeAlgCon eigs = return $ rounded algcon 5
      where algcon = head $ reverse $ init eigs

-- Path Redundancy Computation --

computeNatCon :: [Double] -> IO Double
computeNatCon eigs = return $ rounded natcon 5
      where natcon = log (normal * expsum)
            normal = 1/(fromIntegral $ length eigs)
            expsum = sum $ map (((2.71828 :: Double) ^^) . truncate) eigs

-- Minimum Robustness Computation --

computeCheegerMinBound :: [Double] -> IO Double
computeCheegerMinBound eigs = return $ rounded (specgap/2) 5
              where specgap = head $ sort $ dropWhile (== 0.0) eigs

-- Network Model Estimation --

selectModel :: Double -> Int -> Int -> IO (String, Double)
selectModel c es vs = do
   let algConRanges = [tol, (rounded (max/2) 5), (max - tol)]
          where max = (2 * fromIntegral es)/(fromIntegral vs - 1) -- Fiedler's Bound
                tol = 0.1
   case findIndex (> c) algConRanges of
           Nothing -> return ("Inconclusive", algConRanges !! 1)
           Just a  -> return $ (models !! a, algConRanges !! 1)
                      where models = ["Relatively Disconnected"
                                     ,"Hub and Spoke"
                                     ,"Point to Point"
                                     ]

-- Topological Sorting --

sortTopology :: Graph -> [(String, Int)] -> IO [String]
sortTopology g v = return (map dictLook $ take 4 $ topSort g)
  where dictLook = (\x -> (fromJust $ lookup x (map (\(a,b) -> (b,a)) v)))

revSortTopology :: Graph -> [(String, Int)] -> IO [String]
revSortTopology g v = return (map dictLook $ take 4 $ reverse $ topSort g)
     where dictLook = (\x -> (fromJust $ lookup x (map (\(a,b) -> (b,a)) v)))

-- Analysis Generation --

runAnalyses :: [FilePath] -> [(FilePath, String)] -> IO ()
runAnalyses (x:xs) (y:ys) = do
    fileContents               <- F.parseFile x
    (graph, (no_edges, verts)) <- G.constructGraph fileContents
    laplacian                  <- M.computeL graph (length verts)
    adjacency                  <- M.deriveAMatrix graph (length verts)
    eigenvaluesA               <- M.computeEigs adjacency
    eigenvaluesL               <- M.computeEigs laplacian
    algcon                     <- computeAlgCon eigenvaluesL
    cheeger_min_bound          <- computeCheegerMinBound eigenvaluesL
    natcon                     <- computeNatCon eigenvaluesA
    (model, mean_algcon)       <- selectModel algcon no_edges (length verts)
    top                        <- sortTopology graph verts
    bottom                     <- revSortTopology graph verts
    
    let saturation = rounded ((algcon/mean_algcon) * 100) 5
    let conList    = [algcon, mean_algcon, saturation]
    let connected  = intercalate ", " $ map show conList

    let updateList = [algcon, mean_algcon, saturation, natcon, cheeger_min_bound]
    let updateLine = intercalate ", " $ map show updateList
    let update     = (snd y) ++ ", " ++ updateLine ++ ", " ++ model ++ "\n"

    let result = "\nAirline Network Analysis   " ++ (snd y) ++ "\n"             ++
                 "\nConnectedness:             " ++ "("  ++ connected ++ "%)"   ++
                 "\nPath Redundancy:           " ++ (show natcon)               ++
                 "\nMinimum Robustness:        " ++ (show cheeger_min_bound)    ++
                 "\nNetwork Model Estimation:  " ++ model                       ++
                 "\nHierarchical Topology:     " ++ (show $ top     ++ ["..."]) ++
                 "\n                           " ++ (show $ ["..."] ++ bottom)  ++
                 "\n"

    -- writeFile (fst y) result       # Uncomment to write one file per result
    appendFile "results/overview.txt" result
    appendFile "results/output.csv"   update
    
    print $ "Analyzed: " ++ (snd y)
    runAnalyses xs ys

runAnalyses [] [] = return ()
