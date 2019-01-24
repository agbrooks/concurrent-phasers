{-
This is a stress test of the Phasers that computes a certain generation of
a game of Conway's Game of Life in parallel. The parallelism is very
fine-grained (after all, this is a stress test) -- one process is responsible
for each square in the grid.

There are two "board-shaped" grids of IORefs used. With each generation,
every cell alternates between using one for reading and the other for
writing.

Additionally, there is one "board-shaped" grid of Phasers used for
synchronization. Because each cell depends on the last generation computed
by its neighbors, it uses the phasers at its neighbors' positions in 'Wait' mode,
waiting on their results. Each process uses the phaser in its own position in
'Signal' mode to unblock any processes waiting on its result.

-}
module Control.Concurrent.ConwaySpec
  ( spec
  , main )
where

import Control.Concurrent ( forkIO )
import Control.Concurrent.Phaser
import Control.Concurrent.Phaser.STM
import Control.Monad

import Data.IORef

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = makeSpecForPhaser (newIOPhaser  (0::Int)) "IO Phaser"
    >> makeSpecForPhaser (newSTMPhaser (0::Int)) "STM Phaser"


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

makePhaserBoard :: Phaser p => Int -> (Int -> IO (p Int)) -> IO (PhaserGrid p)
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
    setCell coord = atomicWriteIORef (cell coord size cells) True
    gliderCoords = [(0,0), (1,0), (2,0), (0,1), (1, 2)]
  in sequence_ $ setCell <$> gliderCoords

neighbors :: (Int, Int) -> Int -> [[a]] -> [a]
neighbors (row, col) size board =
  let offsets = filter (/= [0,0]) $ replicateM 2 [-1, 0, 1]
  in (\[i,j] -> cell (row + i, col + j) size board) <$> offsets

nextState :: Bool -> [Bool] -> Bool
nextState this nbrs =
  let aliveSurrounding = length (filter id nbrs)
      computeNext c
        | aliveSurrounding <  2 = False
        | aliveSurrounding == 2 = c
        | aliveSurrounding == 3 = True
        | otherwise = False
  in computeNext this

runConway :: Phaser p => (Int -> IO (p Int)) -> Int -> Int -> IO [[Bool]]
runConway newPhaserImpl size gen =
  let
    coords = replicateM 2 [0..(size - 1)] >>= \[i,j] -> [(i,j)]
  in do
    -- phaser used as a barrier to make sure all parties have finished
    donePh <- newPhaserImpl (size ^ 2 + 1)
    -- set up the cell grids
    phaserGrid <- makePhaserBoard size newPhaserImpl
    oddGrid  <- makeBoard size
    evenGrid <- makeBoard size
    setGlider evenGrid size

    -- compute
    forM_ coords (
      \coord ->
        let oddCell  = cell coord size oddGrid
            evenCell = cell coord size evenGrid
            thisPh   = cell coord size phaserGrid

            nbrPhs   = neighbors coord size phaserGrid
            nbrPairs = map (\p -> (p, Wait)) nbrPhs
            oddNbrs  = neighbors coord size oddGrid
            evenNbrs = neighbors coord size evenGrid
        in forkIO ((replicateM_ gen $ do
             -- Run phased signalling on own phaser, waiting on neighbors
             runMultiPhased ((thisPh, Signal):nbrPairs) $ do
               curPhase <- phase thisPh
               let evenPhase = (curPhase `mod` 2) == 0
                   readCells
                     | evenPhase = evenNbrs
                     | otherwise = oddNbrs
                   writeCell
                     | evenPhase = oddCell
                     | otherwise = evenCell
                   thisCell
                     | evenPhase = evenCell
                     | otherwise = oddCell

               nbrVals <- sequence $ readIORef <$> readCells
               thisVal <- readIORef thisCell
               atomicWriteIORef writeCell (nextState thisVal nbrVals)
           ) >> runPhased donePh Signal (return ()))
     )
    -- wait for all spawned threads to exit
    runPhased donePh Wait $ (return ())
    let
        genEven = gen `mod` 2 == 0
        finalGrid
            | genEven   = evenGrid
            | otherwise = oddGrid
    extractBoard finalGrid

makeSpecForPhaser implNewIntPhaser name = describe name $
  it "produces correct output for the game of life demo" $ do
    let size = 5
    board <- runConway implNewIntPhaser size 150
    -- Since we initialized it with a glider, the result should also be the
    -- glider, but transposed somewhere different on the torus
    board `shouldBe` [[False, False, False, True , False],
                      [False, False, False, False, False],
                      [False, False, False, False, False],
                      [False, False, True , True , True ],
                      [False, False, True , False, False]]
