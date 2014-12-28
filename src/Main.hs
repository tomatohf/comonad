import Automaton
import World
import Zipper

main = mapM_ (putStr . display 3) $ take 10 $ iterate evolve blinker

blinker :: CellularWorld
blinker = World $ Zip fzs (Zip tfs True tfs) fzs
  where
    fzs = repeat fz
    fz  = Zip fs False fs
    tfs = True:fs
    fs  = repeat False
