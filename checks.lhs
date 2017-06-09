> import Board
> import Learning
> import Qlearn
> import Test.QuickCheck
> import Test.QuickCheck.Monadic
> import System.Random

> inConstraints :: (Int, Int) -> Can -> Bool
> inConstraints cst c = cfst c < fst cst && csnd c < snd cst

> genBoard :: (Int, Int) -> Double -> Gen (IO Board)
> genBoard cst p = do   return (randBoard cst p)

 prop_cans_constrained :: (Int, Int) -> Double -> PropertyM

> prop_cans_constrained cst p = forAllM (genBoard cst p)
>                           (\x -> do   let c = x
>                                       b <- run c
>                                       let cs = bsnd b
>                                           ts = map (inConstraints (bfst b)) cs
>                                       assert $ foldr (&&) True ts == True) 