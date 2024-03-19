import Data.List
import Data.Maybe (listToMaybe, mapMaybe)

type Node = Char
type Edge = (Node, Node)
type Graph = ([Node], [Edge])

neighbors :: Node -> [Edge] -> [Node]
neighbors node edges = [y | (x, y) <- edges, x == node] ++ [x | (x, y) <- edges, y == node]

degree :: Node -> [Edge] -> Int
degree node edges = length $ neighbors node edges

sortByDegree :: [Node] -> [Edge] -> [Node]
sortByDegree nodes edges = reverse $ sortOn (\n -> degree n edges) nodes

colorNodes :: [Node] -> [Edge] -> [(Node, Int)]
colorNodes [] _ = []
colorNodes (node:nodes) edges =
    let coloredNeighbors = colorNodes nodes edges
        availableColors = [1..]
        neighborColors :: [Int]
        neighborColors = mapMaybe (\n -> lookup n coloredNeighbors) (neighbors node edges)
        unusedColors = filter (`notElem` neighborColors) availableColors
    in case listToMaybe unusedColors of
        Nothing -> error "Cannot color the graph. All colors are exhausted for a node's neighbors."
        Just color -> (node, color) : coloredNeighbors

kColor :: [Node] -> [Edge] -> [(Node, Int)]
kColor nodes edges = sort $ colorNodes (sortByDegree nodes edges) edges
