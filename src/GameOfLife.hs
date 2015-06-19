module GameOfLife (
    Game,
    newGame,
    setAlive,
    step,
    ) where

import qualified Data.Map as M
import qualified Data.Set as S

data Game = Game (S.Set LivingCell) deriving (Show, Eq)
data LivingCell = LivingCell Int Int deriving (Show, Eq, Ord)
data NeighboringCell = NeighboringCell Int Int deriving (Eq, Ord)
type LivingNeighborCounts = M.Map NeighboringCell Int

newGame :: Game
newGame = Game S.empty

setAlive :: Int -> Int -> Game -> Game
setAlive x y (Game cells) = Game $ S.insert (LivingCell x y) cells

step :: Game -> Game
step (Game cells) = Game newLivingCells
    where allNeighbors = concat $ S.map neighborsOf cells
          neighborCounts = countNeighbors allNeighbors
          newLivingCells = bringToLife $ M.keys $ M.filterWithKey (willLive cells) neighborCounts

neighborsOf :: LivingCell -> [NeighboringCell]
neighborsOf (LivingCell x y) =
    [ NeighboringCell x' y' | x' <- [(x - 1)..(x + 1)],
                              y' <- [(y - 1)..(y + 1)],
                              x /= x' || y /= y' ]

countNeighbors :: [NeighboringCell] -> LivingNeighborCounts
countNeighbors = foldl increment M.empty
    where increment m cell = case M.lookup cell m of
                                 Nothing -> M.insert cell 1 m
                                 Just count -> M.insert cell (count + 1) m

willLive :: S.Set LivingCell -> NeighboringCell -> Int -> Bool
willLive cells neighbor neighborCount =
    (wasAlive && (neighborCount == 2 || neighborCount == 3))
       || (not wasAlive && neighborCount == 3)
    where NeighboringCell x y = neighbor
          wasAlive = (LivingCell x y) `S.member` cells

bringToLife :: [NeighboringCell] -> S.Set LivingCell
bringToLife neighbors = S.fromList $ map (\(NeighboringCell x y) -> LivingCell x y) neighbors
