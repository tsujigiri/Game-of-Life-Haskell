
import GameOfLife

main = do
    print $ step $ setAlive 3 1 $ setAlive 2 1 $ setAlive 1 1 newGame


