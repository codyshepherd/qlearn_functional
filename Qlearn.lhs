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
i: episode number
eps: initial epsilon

> episode                         :: (Int, Int) -> Double -> Int -> Double -> (Qmatrix, Int) -> IO (Qmatrix, Int)
> episode dims p n eps (q, i)     = do  b <- randBoard dims p
>                                       let eps' = if i `mod` 50 == 0 && eps > 0.1 then eps - 0.01 else eps
>                                       (q', b') <- foldr1 (>=>) (replicate n (train eps')) (q,b)
>                                       --print ("episode" ++ show i)
>                                       return (q', i+1)

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



> doTraining            :: (Int, Int) -> Double -> Double -> IO Qmatrix
> doTraining dims p eps = do    (qfinal, i) <- foldr1 (>=>) (replicate n_episodes (episode dims p n_steps eps)) (newQTable,1)
>                               return qfinal


With training conquered, our next task is to conduct testing, wherein the robot makes a 
guess but does not update its Q matrix.

We will rely on the test function from Learning.lhs here.

> testEpisode                       :: (Int, Int) -> Double -> Int -> Double -> (Qmatrix, [Double]) -> IO (Qmatrix, [Double])
> testEpisode dims p n eps (q, r)   = do    b <- randBoard dims p
>                                           (q', b', r') <- foldr1 (>=>) (replicate n (test eps)) (q,b,0.0)
>                                           --print ("testEpisode" ++ show i)
>                                           return (q', (r':r))
>                                           

> doTesting             :: (Int, Int) -> Double -> Double -> Qmatrix -> IO (Qmatrix, [Double])
> doTesting dims p eps q  = do  (q', r) <- foldr1 (>=>) (replicate n_episodes (testEpisode dims p n_steps eps)) (q, [0.0])
>                               return (q', reverse r)