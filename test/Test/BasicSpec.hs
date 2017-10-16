module Test.BasicSpec
  ( spec
  , main )
where

import Control.Concurrent ( forkIO )
import Control.Monad      ( (=<<)
                          , forM_ )
import Test.Hspec
import Test.QuickCheck

import Control.Concurrent.Phaser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Basic Phaser" $ do
    it "Permits registration" $ do
      ph <- newIntPhaser 1
      forM_ [1..100]
        (\i -> do
          register ph
          num_registered <- registered ph
          num_registered `shouldBe` (i + 1)
        )

    it "Permits deregistration" $ do
      ph <- newIntPhaser 100
      forM_ (reverse [1..99])
        (\i -> do
          unregister ph
          num_registered <- registered ph
          num_registered `shouldBe` i
        )

    it "Won't deregister below 1 phaser" $ do
      ph <- newIntPhaser 1
      unregister ph
      num_registered <- registered ph
      num_registered `shouldBe` 1

    it "Won't permit creation of a phaser with less than 1 thread registered" $ do
      ph <- newIntPhaser 0
      num_registered <- registered ph
      num_registered `shouldBe` 1
      ph <- newIntPhaser (-15)
      num_registered <- registered ph
      num_registered `shouldBe` 1

    it "Provides phase when requested" $ do
      -- Int for phase
      the_phase <- phase =<< newPhaser (0 :: Int) 0
      the_phase `shouldBe` 0
      -- Integer for phase
      the_phase <- phase =<< newPhaser (11 :: Integer) 0
      the_phase `shouldBe` 11
      -- Ordering for phase (why not?)
      the_phase <- phase =<< newPhaser LT 0
      the_phase `shouldBe` LT

    it "Advances with one registered party" $ do
      ph <- newIntPhaser 1
      num_registered <- registered ph
      num_registered `shouldBe` 1

      forM_ [1..100]
        (\i -> do
          await ph
          new_phase <- phase ph
          new_phase `shouldBe` i
        )

    it "Advances with multiple registered parties" $ do
      ph <- newIntPhaser 2
      num_registered <- registered ph
      num_registered `shouldBe` 2

      forM_ [1..100]
        (\i -> do
          forkIO (await ph)
          await ph
          new_phase <- phase ph
          new_phase `shouldBe` i
        )
