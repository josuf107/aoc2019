module Graph where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree

type Graph a = Map.Map a (Set.Set a)

addEdge :: Ord a => Graph a -> a -> a -> Graph a
addEdge g a b = Map.insertWith Set.union a (Set.singleton b) g

addEdges ::(Foldable f, Ord a) => Graph a -> f (a, a) -> Graph a
addEdges = foldl (\g (a, b) -> addEdge g a b)

addUndirectedEdge :: Ord a => Graph a -> a -> a -> Graph a
addUndirectedEdge g a b = addEdge (addEdge g a b) b a

addUndirectedEdges :: (Foldable f, Ord a) => Graph a -> f (a, a) -> Graph a
addUndirectedEdges = foldl (\g (a, b) -> addUndirectedEdge g a b)

removeEdge :: Ord a => Graph a -> a -> a -> Graph a
removeEdge g a b = Map.update (removeOrEmpty b) a g

removeUndirectedEdge :: Ord a => Graph a -> a -> a -> Graph a
removeUndirectedEdge g a b = Map.update (removeOrEmpty b) a . Map.update (removeOrEmpty a) b $ g

removeOrEmpty :: Ord a => a -> Set.Set a -> Maybe (Set.Set a)
removeOrEmpty a neighbors =
    let neighbors' = Set.delete a neighbors
    in if Set.null neighbors' then Nothing else Just neighbors'

removeNode :: Ord a => Graph a -> a -> Graph a
removeNode g a = foldl (\g' b -> removeUndirectedEdge g' a b) g (maybe [] Set.toList $ Map.lookup a g)

treeFrom :: Ord a => Graph a -> a -> Tree a
treeFrom g a = unfoldTree step (a, g)
    where
        step :: Ord a => (a, Graph a) -> (a, [(a, Graph a)])
        step (node, graph) 
            = (node, fmap (\n -> (n, removeNode graph node)) . maybe [] Set.toList . Map.lookup node $ graph)

graphFromEdges :: (Foldable f, Ord a) => f (a, a) -> Graph a
graphFromEdges = addEdges Map.empty

undirectedGraphFromEdges :: (Foldable f, Ord a) => f (a, a) -> Graph a
undirectedGraphFromEdges = addUndirectedEdges Map.empty
