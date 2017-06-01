Learning.lhs
Cody Shepherd

> module Learning where

> import System.Random
> import Data.List
> import Data.List.Extras.Argmax
> import Data.List.Utils
> import Data.Ord
> import Board
> import Data.Map (Map)
> import qualified Data.Map as Map


This will be the module that defines the learning algorithm - i.e. the math part.

It will define the functions responsible for creating and updating the Q-Matrix

First, we should define the Q-Matrix and specify how one stores, retrieves, and updates
information contained within it.

The state of a cell is going to be important in the Robot's learning algorithm. The robot needs
to be able to observe the contents of its cell and neighboring cells in order to make a
decision about the best course of action.

Therefore we create named values for different configurations of a state.

> data State    = E
>               | C
>               | W 
>               | Rc
>               | Re
>               deriving (Eq, Show, Ord, Enum)

We also are going to want a quick way to create a "hash" of a given state signature for being
able to store information about a state configuration in the Qmatrix.

> stateKey          :: [State] -> String
> stateKey []       = []
> stateKey (x:xs)   = let nx =  case x of
>                                   E -> '_'
>                                   C -> '.'
>                                   W -> 'w'
>                                   Rc -> '%'
>                                   Re -> 'o'
>                       in nx : stateKey xs

Our Qmatrix is just a simple Mapping

> type Qmatrix = Map String [Float]

Now that we have a notion of a state and state key (a stateKey string plus a list of five Float values),
we need a function that allows Rob to observe his current state.

I have chosen to break this observe function into four separate functions to make it easier to manage
and understand. Ultimately an "observation" results in a stateKey string representing the configuration
of 5 cells that make up the robot's "footprint."

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

Training 
    - Initialize Q(s,a) to all zeros
    - Initialize s
    - selection action a
    - take action a and receive reward r
    - observe new state s'
    - Update Q(s, a) <- Q(s,a) + eta * (r + gamma*argmax(Q(s',a')) - Q(s,a))
    - s <- s'

testStep
    - TODO

In service of either algorithm, the robot needs to be able to select an action 
in a deterministic way, but also get a random action if it wants.

> action :: Int -> IO Dir
> action n =  case n `mod` 6 of
>                0 -> return U
>                1 -> return D
>                2 -> return L
>                3 -> return R
>                4 -> return P
>                5 -> do v <- randomRIO(0,4)
>                        action v

Sometimes this action selection should be random (e.g. when the robot has not learned enough,
or when it wants to maximize exploration over exploitataion). The chances of randomness should be 
based on a tunable value.

> isRandom :: Float -> IO Bool
> isRandom p = do v <- randomRIO(0.0, 1.0)
>                 if v < p then return True else return False

We also want a way to get an action we know or think is good. This requires looking at the Qmatrix
and checking for any "learned knowledge" for the given stateKey state configuration.

> maxI :: [Float] -> Int
> maxI xs = let (f, i) = maximumBy (comparing fst) (zip xs [0..]) in i

> bestAction :: String -> Qmatrix -> IO Dir
> bestAction k q = let l = Map.lookup k q
>                        in case l of
>                            Just n -> action (maxI n)
>                            Nothing -> action 5

In order to perform a single step during training, we need to have the updated states of
the Board and the Qtable; a single step potentially updates both, so it should return them,
I suppose as a pair.


----
DISCUSSION

This should probably be a Monad because it represents a "program" to be run on a set of 
inputs.

 instance Functor Trainer where
   fmap    :: (a -> b) -> Trainer a -> Trainer b
   fmap f T = 

 data Trainer a = T (Qmatrix -> [(Board, Qmatrix)])

 INCONCLUSIVE
 ----

We will need some global constants to control the behavior of our program and keep the number of
copied parameters to a minimum.

> eta = 0.2 :: Float
> gamma = 0.9 :: Float

Training is fundamentally an updating of the board position and the contents of the Qmatrix. Because it
relies on randomness, it becomes an IO function. This creates problems for looping or iterating and accumulating
results in an easy way.

> train         :: (Qmatrix, Board) -> IO (Qmatrix, Board)
> train (q, b) = do           t <- isRandom eta
>                             let s       = observe b
>                             showBoard b
>                             a <- if t then action 5 else bestAction s q
>                             print a
>                             let (s', r) = move a b
>                                 q'      = updateQ q s (observe s') a r
>                             print ("reward " ++ show r)
>                             print ("stateKey: " ++ show s)
>                             print (Map.lookup s q')
>                             return (q', s')

The Qmatrix must be updated according to the Q-Learning algorithm. A more detailed explanation of this algorithm,
and thus what is going on in this functio, will be provided in my paper. 

> updateQ           :: Qmatrix -> String -> String -> Dir -> Float -> Qmatrix
> updateQ q s s' a r   = let qcurrent = Map.lookup s q
>                               in case qcurrent of
>                                   Just sa -> let y = head $ drop (dirIndex a) sa
>                                                  snext = Map.lookup s' q
>                                                  qt = case snext of
>                                                               Just z -> let newval = y + (eta * (r + gamma * (argmax (\x -> x-0.0) z) - y))
>                                                                           in take (dirIndex a) z ++ [newval] ++ drop ((dirIndex a) + 1) z
>                                                               Nothing -> replicate (dirIndex a) 0.0 ++ [y] ++ replicate (4 - (dirIndex a)) 0.0
>                                                  in Map.insert s qt q

We also need a quick way to generate the very first, zero-initialized Qmatrix, based on combinations over the
possible stateKeys. Note that there are some states generated by this algorithm that will never occur, such as
"wwwwo" (a cell surrounded by four walls), but the cost to memory and time is minimal, so we don't really care.

> newQTable :: Map String [Float]
> newQTable = let strings = [ [n] ++ [s] ++ [e] ++ [w] ++ [h] | n <- [E .. W], s <- [E .. W]
>                                                   , e <- [E .. W ], w <- [E .. W], h <- [Rc .. Re]]
>                 in Map.fromList $ map makePair $ map stateKey strings
>                     where makePair x = (x, [0.0,0.0,0.0,0.0,0.0])