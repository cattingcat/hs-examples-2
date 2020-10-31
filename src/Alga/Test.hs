module Alga.Test where

import qualified Algebra.Graph.AdjacencyMap as G
import qualified Algebra.Graph.AdjacencyMap.Algorithm as G


gr :: G.AdjacencyMap Int
gr = G.edges [(1, 2), (2, 3), (2, 4)]

tst1 = G.reachable 1 gr

tst2 = G.bfsForest [1] gr
tst3 = G.bfs [1] gr


--data Tst = Kek {a :: String} | Puk {a :: Int}