import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (cycle)

edgesToAdj :: Ord a => [(a,a)] -> M.Map a [a]
edgesToAdj edges = foldr (\(u, v) acc -> M.insertWith (++) u [v] acc) M.empty edges


dfs :: Ord a => M.Map a [a] -> a -> S.Set a -> [a] -> a -> [[a]] 
dfs adj u visited current target = if S.member u visited 
                                then
                                    if target == u 
                                    then   
                                        [current++[u]]
                                    else 
                                        []
                                else
                                    let newVisited = S.insert u visited
                                        newCurrent = current++[u]
                                    in concatMap (\v ->  dfs adj v newVisited newCurrent target) (M.findWithDefault [] u adj)  


cycle :: Ord a => a -> [(a,a)] -> [[a]]
cycle u edges = dfs (edgesToAdj edges) u S.empty [] u