import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (forM_)

paths :: Ord a => a -> a -> [(a,a)] -> [[a]]
paths f t edges = dfs f  (edgesToAdj edges) S.empty t []


edgesToAdj :: Ord a => [(a,a)] -> M.Map a [a]
edgesToAdj edges = foldr (\(u, v) acc ->
                        let updatedAcc = M.insertWith (++) u [v] acc
                         in updatedAcc) M.empty edges


dfs :: (Ord a) => a -> M.Map a [a] -> S.Set a -> a -> [a] -> [[a]] 
dfs u adj visited target current = if u == target
                                        then [current++[target]]
                                        else
                                            let newVisited = S.insert u visited
                                                newCurrent = current++[u]
                                            in concatMap (\v -> if S.notMember v newVisited
                                                                    then dfs v adj newVisited target newCurrent
                                                                    else []
                                                        ) (M.findWithDefault [] u adj)  