Learning.lhs
Cody Shepherd

> module Learning where

> import System.Random
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


Training 
    - Initialize Q(s,a) to all zeros
    - Initialize s
    - selection action a
    - take action a and receive reward r
    - observe new state s'
    - Update Q(s, a) <- Q(s,a) + eta * (r + gamma*argmax(Q(s',a')) - Q(s,a))
    - s <- s'

Need to be able to select an action in a deterministic way, but also get a random action if
we want.

> action :: Int -> IO Dir
> action n =  case n `mod` 6 of
>                0 -> U
>                1 -> D
>                2 -> L
>                3 -> R
>                4 -> P
>                5 -> do v <- RandomRIO(0,4)
>                        action v

Sometimes this action selection should be random. The chances of randomness should be 
based on a probability.

> isRandom :: Float -> IO Bool
> isRandom p = do v <- RandomRIO(0.0, 1.0)
>                 if v < p then True else False

We also want a way to get an action we know or think is good.

> bestAction :: String -> Qmatrix -> IO Dir
> bestAction k q = let l = Map.lookup k q
>                        in case l of
>                            Just n -> action (maxI n)
>                            Nothing -> action 5

In order to perform a single step during training, we need to have the updated states of
the Board and the Qtable; a single step potentially updates both, so it should return them,
I suppose as a pair.

This should probably be a Monad because it represents a "program" to be run on a set of 
inputs.

 instance Functor Trainer where
   fmap    :: (a -> b) -> Trainer a -> Trainer b
   fmap f T = 

 data Trainer a = T (Qmatrix -> [(Board, Qmatrix)])

We might need some global constants to control the behavior of our program and keep the number of
copied parameters to a minimum.

n_global = 5000
m_global = 200
eta = 0.2 :: Float
gamma = 0.9 :: Float

> train         :: Qmatrix -> Board -> Float -> IO (Qmatrix, Board)
> train q b p    = do s <- observe b
>                     a <- if isRandom p then action 5 else bestAction k q