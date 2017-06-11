Qlearn.lhs
Cody Shepherd

> module Qlearn where

> import Learning
> import Board
> import Data.List
> import Control.Monad
> import System.Random
> import Data.Map (Map)
> import qualified Data.Map as Map

The key piece in this project that eluded my during my last attempt was 
being able to "loop" a given number of times, repeating a computation and
accumulating and updated model.

In the definition of this algorithm we define an episode of 200 training 
steps during which the Qmatrix model is updated and accumulated; at the end
of the episode the board is thrown out but the Qmatrix is kept for further
training.

The Kleisli composition operator is going to facilitate this for us.

First we need some constants in order to keep function parameters minimal.
Ultimately it would be nice to specify these at runtime from a terminal, but
that's a project for another day.

> n_steps = 200
> n_episodes = 5000


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

The episode function conducts a training episode. It uses the Kleisli
operator to pass the results of each training step from one step to 
the next, and come out at the end with the final accumulated result.

dims: dimensions of board (excluding wall border)
p: % of cells with cans at start
n: number of steps in episode
eps: epsilon value
q: zero-initialized Qmatrix
i: episode number
r: The rewards list from the previous call to episode, to be extended

> episode                         :: (Int, Int) -> Double -> Int -> Double -> (Qmatrix, Int, [Double]) -> IO (Qmatrix, Int, [Double])
> episode dims p n eps (q, i, r)  = do  b <- randBoard dims p
>                                       let eps' = if i `mod` 50 == 0 && eps > 0.1 then eps - 0.01 else eps
>                                       (q', b') <- foldr1 (>=>) (replicate n (train eps')) (q,b)
>                                       --print ("episode" ++ show i)
>                                       let r' = sum $ map sum (Map.elems q')
>                                       return (q', i+1, r':r)

doTraining conducts the entire training phase by folding the Kleisli 
operator over episodes.

dims: dimensions of board (excluding wall border)
p: % of cells with cans at start
eps: initial epsilon value, to be annealed

> doTraining            :: (Int, Int) -> Double -> Double -> IO (Qmatrix, [Double])
> doTraining dims p eps = do    (qfinal, i, rs) <- foldr1 (>=>) (replicate n_episodes (episode dims p n_steps eps)) (newQTable,1, [0])
>                               return (qfinal, reverse rs)


With training conquered, our next task is to conduct testing, wherein the robot makes a 
guess but does not update its Q matrix.

These functions work in the same way as the training functions.

dims: the board dimensions, as above 
p: the percentage of cans on the board 
n: the number of test episodes to run 
eps: the epsilon value (remains constant during testing)
q: the trained Q Table 
r: The rewards list from the last call to testEpisode, to be extended

> testEpisode                       :: (Int, Int) -> Double -> Int -> Double -> (Qmatrix, [Double]) -> IO (Qmatrix, [Double])
> testEpisode dims p n eps (q, r)   = do    b <- randBoard dims p
>                                           (q', b', r') <- foldr1 (>=>) (replicate n (test eps)) (q,b,0.0)
>                                           --print ("testEpisode" ++ show i)
>                                           return (q', (r':r))
>                                           

The parameters for this function are the same as above.

> doTesting             :: (Int, Int) -> Double -> Double -> Qmatrix -> IO (Qmatrix, [Double])
> doTesting dims p eps q  = do  (q', r) <- foldr1 (>=>) (replicate n_episodes (testEpisode dims p n_steps eps)) (q, [0.0])
>                               return (q', reverse r)