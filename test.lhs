
> module Main(main) where
> import Board
> import Learning
> import Qlearn
> import qualified Data.Map as Map
> import Data.List
> import Graphics.Rendering.Chart.Easy
> import Graphics.Rendering.Chart.Backend.Cairo


What I'd like to do here is set up my initial board, set up my zero-initialized Qmatrix, 
and then "iterate" train on those initial parameters, passing its results into the next
computation, for some number of times. I.e. I would like to "accumulate" the results of
each successive call to train.

Currently this does not work because train requires a (Qmatrix, Board) pair as input, but
(by necessity) outputs type IO(Qmatrix, Board). Still working on a way around this. I may
resort to converting the train computation to a Monad and writing a custom iterateM function,
but maybe that's not necessary?

> zipInd    :: [Double] -> [(Int, Double)]
> zipInd rs = [0..] `zip` rs

> average xs = realToFrac (sum xs) / genericLength xs

> main = do putStrLn ("Training...")
>           (qfinal, rwds) <- doTraining (8,8) 0.5 1.0
>           putStrLn ("Training finished.\nResulting qmatrix:")
>           print qfinal
>           putStrLn ("Testing...")
>           (q', rs) <- doTesting (8,8) 0.5 0.1 qfinal
>           putStrLn ("Testing finished.\nAverage rewards over episodes:")
>           print $ average rs
>           toFile def "rewards_eps0_1.png" $ do
>               layout_title .= "Rewards over episodes"
>               plot (line "eps=0.1" [zipInd rs])
>               return ()


            let b = Board ((8,8), [Can (2,3), Can (3,4), Can (3, 6)], Rob(3, 3))
               q = newQTable
               l = take 10 $ iterate train (q, b)
           return ()



OLD TESTING


               b' = move U b
               b'' = move R b
           showBoard b
           print (observe b)
           showBoard b'
           print (observe b')
           showBoard b''
           print (observe b'')


               k = Key (stateKey s)
               v = Values [0,0,0,0,0]
               qt = Map.fromList [(k,v)]
               qt' = Map.insert (Key (stateKey [E, E, E, E, C])) (Values [1, 0,0,0,0]) qt
           print qt'