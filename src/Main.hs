module Main where

import Control.Concurrent ( threadDelay )
import GHC.Conc (getNumCapabilities)

import Control.Monad.Par.IO ( runParIO, ParIO, IVar )
-- NOTE: must be Control.Monad.Par.Class not Control.Monad.Par
import Control.Monad.Par.Class ( get, put, new, fork, spawn )
import Control.Monad.IO.Class ( MonadIO(liftIO) )


expensiveIOComputation :: Int -> IO Int
expensiveIOComputation x = do
  putStrLn "expensiveIOComputation called"
  threadDelay 2000000
  pure $ x + 1

main :: IO ()
main = do
  getNumCapabilities >>= (\x -> putStrLn ("Number of cores: " <> show x))
  x <- runParIO parMain
  print x

parMain :: ParIO [Int]
parMain = do
  -- Start with a list of IO actions
  let computations :: [IO Int]
      computations = map expensiveIOComputation [1, 2, 3, 4]
  -- Lift the computations to the ParIO monad (required for `spawn` to work)
  let liftedComputations :: [ParIO Int]
      liftedComputations = map liftIO computations
  -- Arrange each action to run in parallel
  let computationsToRun :: ParIO [IVar Int]
      computationsToRun = mapM spawn liftedComputations
  -- runningComputations :: [IVar Int]
  runningComputations <- computationsToRun
  -- Wait for all computations to finish before returning all the results
  -- by getting the results in the IVars
  -- get :: IVar Int -> ParIO Int
  mapM get runningComputations
