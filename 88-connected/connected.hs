import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')

initAdjMap :: Ord a => [a] -> M.Map a [a]
initAdjMap []       = M.empty
initAdjMap (x:xs)   = M.insert x [] (initAdjMap xs)

addEdges :: Ord a => M.Map a [a] -> [(a, a)] -> M.Map a [a]
addEdges graph edges = foldr (\(u, v) acc ->
    let updatedAcc = M.insertWith (++) u [v] acc
        updatedAcc' = M.insertWith (++) v [u] updatedAcc
    in updatedAcc') graph edges

adjList :: Ord a => ([a],[(a,a)]) -> M.Map a [a]
adjList (nodes, edges) = addEdges (initAdjMap nodes) edges

dfs :: (Ord a) => a -> M.Map a [a] -> S.Set a -> ([a], S.Set a)
dfs u adj visited
    | u `S.member` visited = ([], visited)
    | otherwise =
        let newVisited = S.insert u visited
            (dfsResult, finalVisited) = foldl' processNeighbor ([], newVisited) (M.findWithDefault [] u adj)
        in (u : dfsResult, finalVisited)
  where
    processNeighbor (acc, visited') v
        | v `S.member` visited' = (acc, visited')
        | otherwise =
            let (neighborDfsResult, updatedVisited) = dfs v adj visited'
            in (acc ++ neighborDfsResult, updatedVisited)

depthFirst :: Ord a => ([a],[(a,a)]) -> [a] -> S.Set a -> [[a]]
depthFirst graph (x:xs) visited = if cc /= [] 
                                    then 
                                        [cc] ++ depthFirst graph xs newVisited
                                    else 
                                        depthFirst graph xs newVisited 
                                where (cc, newVisited) = dfs x (adjList graph) visited
depthFirst graph [] _ = []


connectedComponents :: Ord a => ([a],[(a,a)]) -> [[a]]
connectedComponents graph = depthFirst graph (fst graph) S.empty 