# Dead simple parallel IO demo for Haskell

Concise sample (see `src/Main.hs` for comments and types):

```hs
expensiveIOComputation :: Int -> IO Int
expensiveIOComputation x = do
  putStrLn "expensiveIOComputation called"
  threadDelay 2000000
  pure $ x + 1

main :: IO ()
main = do
  x <- runParIO parMain
  print x

parMain :: ParIO [Int]
parMain = do
  let computations = map expensiveIOComputation [1, 2, 3, 4]
  runningComputations <- mapM runInFork computations
  mapM get runningComputations

runInFork :: IO Int -> ParIO (IVar Int)
runInFork f = do
  v <- new
  fork (liftIO f >>= put v)
  pure v
```

## Dependencies
- monad-par
- abstract-par

## General tips
- Compile with `-threaded`
- Run the executable with `+RTS -N2` (this means 2 cores; change the number to suit your needs)

## Usage

1. Git clone
2. `stack build`
3. `stack install`
4. `~/.local/bin/parallelIO +RTS -N2`
