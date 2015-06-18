import Test.Hspec
import GameOfLife

main :: IO ()
main = hspec $ do
    describe "blinker" $ do
        it "blinks" $ do
            let game = initGame [(1, 1), (1, 2), (1, 3)] in
                (step game) `shouldBe` initGame [(0, 2), (1, 2), (2, 2)]

    describe "block" $ do
        it "stays as is" $ do
            let game = initGame [(1, 1), (1, 2), (2, 1), (2, 2)] in
                step game `shouldBe` game

initGame :: [(Int, Int)] -> Game
initGame = foldl (\game (x, y) -> setAlive x y game) newGame
