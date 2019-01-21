module Control.Concurrent.ConwaySpec
  ( spec
  , main )
where

import Control.Concurrent ( forkIO )
import Control.Concurrent.Phaser
import Control.Concurrent.Phaser.STM
import Control.Monad

import Data.IORef
import Data.List

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =  makeSpecForPhaser (newSTMPhaser (0::Int)) "STM Phaser"
     >> makeSpecForPhaser (newIOPhaser  (0::Int)) "IOPhaser"


type Cell = IORef Bool
type Cells = [[IORef Bool]]
type PhaserGrid p = [[p Int]]

{-
A mutable game of life board, with the state of each cell being
contained in an IORef.

If you're thinking that this would be better off as a vector or an array,
you would be right. However, I don't want to pull in the vector package
just for a test, or do something as tedious and unsafe as an array.
-}
makeBoard :: Int -> IO Cells
makeBoard size = replicateM size $ replicateM size (newIORef False)

makePhaserBoard :: Phaser p => Int -> (Int -> IO (p Int)) -> IO PhaserGrid
makePhaserBoard size newPhaserImpl =
  replicateM size $ replicateM size (newPhaserImpl 9)


extractBoard :: [[IORef a]] -> IO [[a]]
extractBoard board =
  let extractRow row = sequence $ (readIORef <$> row)
  in sequence $ map (extractRow) board

cell :: (Int, Int) -> Int -> [[a]] -> a
cell (row, col) size board = (board !! (row `mod` size)) !! (col `mod` size)

-- Mark the cells forming a "glider" as alive
setGlider :: Cells -> Int -> IO ()
setGlider cells size =
  let
    setCell coord = writeIORef' (cell coord size cells) True
    gliderCoords = [(0,0), (1,0), (2,0), (0,1) (1, 2)]
  in sequence $ setCell <$> gliderCoords

neighbors :: (Int, Int) -> Int -> [[a]] -> [a]
neighbors (row, col) size board =
  let offsets = filter (/= [0,0]) $ replicateM 2 [-1, 0, 1]
  in (\[i,j] -> cell (row + i, col + j) size board) <$> offsets

nextState :: Bool -> [Bool] -> Bool
nextState this neighbors
  let aliveSurrounding = length (filter id neighbors)
      compute c
        | aliveSurrounding <  2 = False
        | aliveSurrounding == 3 = True
        | aliveSurrounding == 4 = c
        | otherwise = False

runConway newPhaserImpl size gen =
  let
    coords = replicateM 2 [0..(size - 1)]
  in do
    -- phaser to block all parties until they've finished
    donePh <- newPhaserImpl (size ^ size)
    -- set up the cell grids
    phaserGrid <- makePhaserBoard size newPhaserImpl
    oddGrid  <- makeBoard size
    evenGrid <- makeBoard size
    setGlider evenGrid

    -- compute
    forM coords (
      \coord ->
        let oddCell  = cell coord size oddGrid
            evenCell = cell coord size evenGrid
            thisPh   = cell coord size phaserGrid

            nbrPhs   = neighbors coord size phaserGrid
            nbrPairs = TODO
            oddNbrs  = neighbors coord size oddGrid
            evenNbrs = neighbors coord size evenGrid
        in forkIO (replicateM gen $ do
             runMultiPhased (thisPh, Signal):nbrPairs $ do
               curPhase <- phase thisPh
               let evenPhase = (curPhase `mod` 2) == 0
                   readCells
                     | evenPhase = evenNbrs
                     | otherwise = oddNbrs
                   writeCell
                     | evenPhase = oddCell
                     | otherwise = evenCell

                   -- TODO: READ THE IOREFS
                   -- TODO: FIND THE NEXT VALUE
                   -- TODO: UPDATE THE CELL
                   
                x <- undefined

             -- Done!
             runPhased donePh SignalWait (return ())
           )
    )
    runPhased donePh Wait $ do
      undefined -- TODO



makeSpecForPhaser implNewIntPhaser name = describe name $ do
  -- TODO fill out the placeholder test to make sure that the conway test is correct.
  it "produces correct output for the game of life demo" $ do
    1 `shouldBe` 1
