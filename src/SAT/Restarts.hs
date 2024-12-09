module SAT.Restarts (computeNextLubyThreshold, luby, increaseLubyCount) where 
import SAT.Monad (SolverM, lubyCount)
import Control.Monad.RWS (modify)

computeNextLubyThreshold :: Int -> Int
computeNextLubyThreshold count =
  let scale = 10
  in scale * luby count

luby :: Int -> Int
luby k = go k 1 1
  where
    go 1 _ _ = 1
    go n power level
      | n == power + level - 1 = level
      | n < power + level - 1  = go n power (level `div` 2)
      | otherwise              = go n (power * 2) (level * 2)

increaseLubyCount :: SolverM ()
increaseLubyCount = modify $ \s -> s { lubyCount = lubyCount s + 1 }
