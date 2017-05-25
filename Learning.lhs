Learning.lhs
Cody Shepherd

> module Learning where

> import Board
> import Data.Map (Map)


This will be the module that defines the learning algorithm - i.e. the math part.

It will define the functions responsible for creating and updating the Q-Matrix

First, we should define the Q-Matrix and specify how one stores, retrieves, and updates
information contained within it.

> data State    = E
>               | C
>               | W 
>               | Rc
>               | Re
>               deriving (Eq, Show, Ord)

> stateKey          :: [State] -> String
> stateKey []       = []
> stateKey (x:xs)   = let nx =  case x of
>                                   E -> '_'
>                                   C -> '.'
>                                   W -> 'w'
>                                   Rc -> '%'
>                                   Re -> 'o'
>                       in nx : stateKey xs

> newtype Key      = Key String
>                       deriving (Eq, Show, Ord)

> newtype Values    = Values [Float]
>               deriving (Eq, Show, Ord)

> newtype Qmatrix = Qmatrix (Map Key Values)
>                      deriving (Eq, Show)

Fundamentally, Q-Learning requires two parts: training and testing. These parts differ
in that the q-matrix may be updated during training, but not during testing.

trainStep

testStep