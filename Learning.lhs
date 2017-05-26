Learning.lhs
Cody Shepherd

> module Learning where

> import Board
> import Data.Map (Map)


This will be the module that defines the learning algorithm - i.e. the math part.

It will define the functions responsible for creating and updating the Q-Matrix

First, we should define the Q-Matrix and specify how one stores, retrieves, and updates
information contained within it.

The state of a cell is going to be important in the Robot's learning algorithm. It needs
to be able to observe the contents of its cell and neighboring cells in order to make a
decision about the best course of action.

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

> type Qmatrix = Map Key Values

                      deriving (Eq, Show)

Now that we have a notion of a state and state key, we need a function that allows Rob to 
observe his current state.

> obsU        :: Board -> State
> obsU (Board ((a, b), cs, r))
>                            | fst rx == a-1 = W
>                            | any (\x -> (cfst x == (fst rx)+1) && (csnd x == snd rx)) cs = C
>                            | otherwise = E
>                            where 
>                               rx = rToPair r

> obsD        :: Board -> State
> obsD (Board ((a, b), cs, r))
>                           | fst rx == 0 = W
>                           | any (\x -> (cfst x == (fst rx)-1) && (csnd x == snd rx)) cs = C
>                           | otherwise = E
>                           where
>                               rx = rToPair r

> obsE          :: Board -> State 
> obsE (Board ((a, b), cs, r))
>                           | snd rx == b-1 = W
>                           | any (\x -> (csnd x == (snd rx)+1) && (cfst x == fst rx)) cs = C
>                           | otherwise = E
>                           where
>                               rx = rToPair r

> obsW          :: Board -> State 
> obsW (Board ((a, b), cs, r))
>                           | snd rx == 0 = W
>                           | any (\x -> (csnd x == (snd rx)-1) && (cfst x == fst rx)) cs = C
>                           | otherwise = E
>                           where
>                               rx = rToPair r


> obsH          :: Board -> State 
> obsH (Board ((a, b), cs, r))
>                           | any (\x -> (csnd x == snd rx) && (cfst x == fst rx)) cs = Rc
>                           | otherwise = Re
>                           where
>                               rx = rToPair r

> observe       :: Board -> String
> observe b   = let  n = obsU b
>                    s = obsD b
>                    e = obsE b
>                    w = obsW b
>                    h = obsH b
>                    in stateKey [n, s, e, w, h]

Fundamentally, Q-Learning requires two parts: training and testing. These parts differ
in that the q-matrix may be updated during training, but not during testing.

trainStep

testStep