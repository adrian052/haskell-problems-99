import qualified Data.Map as M
import qualified Data.List as L

data Graph a = Graph { nodes :: [a]
                     , edges :: [(a, a)]
                     } deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
            deriving (Show, Eq)


graph = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]


initAdjMap :: Ord a => [a] -> M.Map a [a]
initAdjMap []       = M.empty
initAdjMap (x:xs)   = M.insert x [] (initAdjMap xs)


addEdges :: Ord a => M.Map a [a] -> [(a, a)] -> M.Map a [a]
addEdges graph edges = foldr (\(u, v) acc ->
    let updatedAcc = M.insertWith (++) u [v] acc
        updatedAcc' = M.insertWith (++) v [u] updatedAcc
    in updatedAcc') graph edges

graphToAdj :: Ord a => Graph a -> Adjacency a
graphToAdj g = Adj (M.toList (addEdges (initAdjMap $ nodes g) (edges g)))