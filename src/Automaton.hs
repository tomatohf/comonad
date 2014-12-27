module Automaton where

import World
import Comonad

type Cellular = Bool
type CellularWorld = World Cellular

-- Conway's Game of Life
-- http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
rule :: CellularWorld -> Cellular
rule world = case length alives of
    2 -> extract world
    3 -> True
    _ -> False
  where
    alives = filter (True ==) neighbours
    neighbours = fmap neighbour moves
    neighbour move = extract $ move world
    moves = [ up
            , down
            , left
            , right
            , up . left
            , up . right
            , down . left
            , down . right
            ]

evolve :: CellularWorld -> CellularWorld
evolve = extend rule
