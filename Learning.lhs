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
>               deriving (Eq, Show)

> data Key      = Key (State, State, State, State, State)

> data Values = Values (Float, Float, Float, Float, Float)
>               deriving (Eq, Show)

> newtype Qmatrix = Qmatrix (Map State Values)
>                      deriving (Eq, Show)

Fundamentally, Q-Learning requires two parts: training and testing. These parts differ
in that the q-matrix may be updated during training, but not during testing.

trainStep

testStep