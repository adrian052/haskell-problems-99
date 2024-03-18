import qualified Data.Map as M
import qualified Data.Set as S

data Color = Red | Blue deriving (Eq, Show)

initAdjMap :: Ord a => [a] -> M.Map a [a]
initAdjMap = M.fromList . map (\x -> (x, []))

addEdges :: Ord a => M.Map a [a] -> [(a, a)] -> M.Map a [a]
addEdges graph edges = foldr (\(u, v) acc ->
    let updatedAcc = M.insertWith (++) u [v] acc
        updatedAcc' = M.insertWith (++) v [u] updatedAcc
    in updatedAcc') graph edges

adjList :: Ord a => ([a],[(a,a)]) -> M.Map a [a]
adjList (nodes, edges) = addEdges (initAdjMap nodes) edges

dfs :: (Ord a) => a -> M.Map a [a] -> M.Map a Color -> Color -> Bool
dfs u adj visited color = case M.lookup u visited of
    Just c -> c == color
    Nothing -> let newVisited = M.insert u color visited
                   nextColor = if color == Red then Blue else Red
               in all (\v -> dfs v adj newVisited nextColor) (M.findWithDefault [] u adj)

bipartite :: Ord a => ([a], [(a,a)]) -> Bool
bipartite (nodes, edges) = all (\n -> dfs n adj M.empty Red) nodes
    where adj = adjList (nodes, edges)
