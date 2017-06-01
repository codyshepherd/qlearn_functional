Cody Shepherd

> module Qlearn where

> import Learning
> import Board
> import Data.List
> import Control.Monad
> import System.Random

The key piece in this project that eluded my during my last attempt was 
being able to "loop" a given number of times, repeating a computation and
accumulating and updated model.

In the definition of this algorithm we define an episode of 200 training 
steps during which the Qmatrix model is updated and accumulated; at the end
of the episode the board is thrown out but the Qmatrix is kept for further
training.

The Kleisli composition operator is going to facilitate this for us.


> episode       :: (Int, Int) -> Double -> Int -> Qmatrix -> IO Qmatrix
> episode dims p n q     = do   b <- randBoard dims p
>                               (q', b') <- foldr1 (>=>) (replicate n train) (q,b)
>                               return q'

To facilitate the above function we need to be able to randomly generate a 
starting board

> randBoard         :: (Int, Int) -> Double -> IO Board
> randBoard dims p  = do    let n = floor $ fromIntegral (fst dims) * fromIntegral (snd dims) * p
>                           cs <- randCans dims n
>                           r <- randRob dims
>                           return (Board(dims, cs, r))

> randCans          :: (Int, Int) -> Int -> IO [Can]
> randCans dims 0   = return []
> randCans dims n   = do   i <- randomRIO(0, (fst dims)-1)
>                          j <- randomRIO(0, (snd dims)-1)
>                          let c = Can(i, j)
>                          cs <- randCans dims (n-1)
>                          return $ nub (c:cs)

> randRob           :: (Int, Int) -> IO Rob
> randRob dims      = do    i <- randomRIO(0, (fst dims)-1)
>                           j <- randomRIO(0, (snd dims)-1)
>                           let r = Rob(i, j)
>                           return r