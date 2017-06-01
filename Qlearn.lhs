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

> n_steps = 200
> n_episodes = 5000

i: which episode this is
dims: dimensions of board (excluding wall border)
p: % of cells with cans at start
n: number of steps in episode
q: zero-initialized Qmatrix

> episode       :: (Int, Int) -> Double -> Int -> (Qmatrix, Int) -> IO (Qmatrix, Int)
> episode dims p n (q, i)     = do  b <- randBoard dims p
>                                   (q', b') <- foldr1 (>=>) (replicate n train) (q,b)
>                                   --print ("episode" ++ show i)
>                                   return (q', i+1)

To facilitate the above function we need to be able to randomly generate a 
starting board

dims: dimensions of board (excluding wall border)
p: % of cells with cans at start

> randBoard         :: (Int, Int) -> Double -> IO Board
> randBoard dims p  = do    let n = floor $ fromIntegral (fst dims) * fromIntegral (snd dims) * p
>                           cs <- randCans dims n
>                           r <- randRob dims
>                           return (Board(dims, cs, r))

dims: dimensions of board (excluding wall border)
n: number of cans desired

> randCans          :: (Int, Int) -> Int -> IO [Can]
> randCans dims 0   = return []
> randCans dims n   = do   i <- randomRIO(0, (fst dims)-1)
>                          j <- randomRIO(0, (snd dims)-1)
>                          let c = Can(i, j)
>                          cs <- randCans dims (n-1)
>                          return $ nub (c:cs)

dims: dimensions of board (excluding wall border)

> randRob           :: (Int, Int) -> IO Rob
> randRob dims      = do    i <- randomRIO(0, (fst dims)-1)
>                           j <- randomRIO(0, (snd dims)-1)
>                           let r = Rob(i, j)
>                           return r



> doTraining        :: (Int, Int) -> Double -> IO Qmatrix
> doTraining dims p = do    (qfinal, i) <- foldr1 (>=>) (replicate n_episodes (episode dims p n_steps)) (newQTable,1)
>                           return qfinal