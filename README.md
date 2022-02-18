# Dead simple parallel IO demo for Haskell

Concise sample (see `src/Main.hs` for comments and types):

```hs
import Control.Monad.Par.IO ( runParIO, ParIO, IVar )
-- NOTE: must be Control.Monad.Par.Class not Control.Monad.Par
import Control.Monad.Par.Class ( get, put, new, fork )

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

## Extra tips

- `runInFork` here is monomorphic and specialized to `Int`s, but it can be generic:

```hs
runInFork :: (ParIVar ivar m, MonadIO m, NFData a) => IO a -> m (ivar a)
runInFork f = do
  v <- new
  fork (liftIO f >>= put v)
  pure v
```

The most important requirement is that your "inner result data" (`a`) must be `NFData`, meaning it must be able to be evaluated to normal form (fully evaluated with no remaining constructors). Common data types are already `NFData`, see https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html#t:NFData

`runInFork` had type `IO Int -> ParIO (IVar Int)`, but it can also be `IO String -> ParIO (IVar String)`, etc.

- For custom newtypes that wrap `NFData` types, try:

```hs
import Control.Monad.Par.Class (NFData) -- new

newtype Raster = Raster String
  deriving (Generic)  -- new

instance NFData Raster -- new
```

## Further reading and sources

- [Parallel and Concurrent Programming in Haskell by Simon Marlow](https://learning.oreilly.com/library/view/parallel-and-concurrent/9781449335939/)
- [monad-par](https://hackage.haskell.org/package/monad-par)
- [parallel](https://hackage.haskell.org/package/parallel-3.2.2.0/)
