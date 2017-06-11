
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

I've written a couple of helper functions here to assist with plotting the results of 
the experiment as desired.

> zipInd    :: [Double] -> [(Int, Double)]
> zipInd rs = [0..] `zip` rs

> average xs = realToFrac (sum xs) / genericLength xs

> every n xs = case drop (n-1) xs of
>               (y:ys) -> y : every n ys
>               [] -> []

> main = do putStrLn ("Training...")
>           (qfinal, rwds) <- doTraining (8,8) 0.5 1.0
>           putStrLn ("Training finished.")
>           --print qfinal
>           putStrLn ("Testing...")
>           (q', rs) <- doTesting (8,8) 0.5 0.1 qfinal
>           putStrLn ("Testing finished.")
>           --print $ average rs
>           let tot = foldr (+) 0.0 rs
>               avg = tot / (fromIntegral $ length rs)
>               epis = every 100 rwds
>           toFile def "rewards_eps0_1.png" $ do
>               layout_title .= "Test Reward Avg" ++ (show avg)
>               layout_x_axis . laxis_title .= "Episode (x 100)"
>               layout_y_axis . laxis_title .= "Reward in Q Table"
>               plot (line "eps=0.1" [zipInd epis])
>               return ()
