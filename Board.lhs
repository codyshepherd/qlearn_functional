Cody Shepherd
Q-Learning with Haskell

> module Board where
> import Math.Geometry.Grid
> import Math.Geometry.Grid.Square

The Rob type is an instance of Rob's location on the board. We would like to 
be able to compare this location to a pair of Ints later on, so we will provide a couple
of conversion functions.

> data Rob = Rob (Int, Int)
>               deriving (Show, Eq)

> rfst      :: Rob -> Int
> rfst (Rob (a, b)) = a

> rsnd      :: Rob -> Int
> rsnd (Rob (a, b)) = b

> rToPair   :: Rob -> (Int, Int)
> rToPair (Rob a) = a

The first thing Rob has to do is move around a board. 
It seems like we don't need to store all the empty locations... we can just
track where there are cans. A can, like Rob, is defined by its location.

A location will be defined in its implementation as (a, b), where a represents the "row"
or y-axis location, and b will represent the "column" or x-axis location. This is reversed
from the format of cartesian coordinates because this version makes it much easier to 
use in the context of lists of lists - a being the number of the outermost list, and b 
being the offset within that list. 

However, this format will behave as expected (as a cartesian pair) when visualized. I.e. specifying
Rob's location at (0, 0) will put him at the bottom left corner of the grid, and moving him up will
result in a location of (0, 1). This should turn out to not matter much in the actual execution of
the q-learning algorithm, as it is primarily concerned with the "key" of each grid square rather 
than its grid coordinates.

> data Can = Can (Int, Int)
>            deriving (Show, Eq)

> cfst       :: Can -> Int
> cfst (Can (a, b)) = a

> csnd          :: Can -> Int
> csnd (Can (a, b)) = b

> cToPair       :: Can -> (Int, Int)
> cToPair (Can a) = a


 instance Functor Can where
 fmap f (Can x) = Can (f x)


For the board, we will have to define limits, but otherwise we don't need to store 
empty cells.

ATM we define Board as a 3-tuple with its height and width limits, pluts a list
of cans within it: ((MAXHEIGHT, MAXWIDTH), Cans)

It might be more convenient to use the RectSquareGrid type built into the grid package
than to try to whip up our own board type. We'll try that for now.

> data Board = Board ((Int, Int), [Can], Rob)
>               deriving (Eq, Show)

> bfst      :: Board -> (Int, Int)
> bfst (Board (a, b, c)) = a

> bsnd      :: Board -> [Can]
> bsnd (Board (a, b, c)) = b

> bthd      :: Board -> Rob
> bthd (Board (a, b, c)) = c

 build     :: Int -> Int -> Board
 build x y = Board (rectSquareGrid x y)

If Rob is going to move, we would like some way of constraining his possible movements
to the cardinal directions. We'll define a direction in the order of "nsew", though it 
doesn't make any difference.

> data Dir =  U
>           | D
>           | R
>           | L
>           deriving (Show, Eq)

We want a nice way to visualize the board.

First we need to break up a list of "[ ]" cell tokens into a list of n cell tokens, delimited
by newlines:

> splitEvery :: Int -> [String] -> [String]
> splitEvery _ [] = []
> splitEvery n s = take n s ++ "\n" : splitEvery n ss
>                   where ss = drop n s

Then we will process the list of cans into their places on the "grid" and build a graphical
representation of the grid for printing.

> showBoard     :: Board -> IO()
> showBoard b     = do let r = (rfst (bthd b), rsnd (bthd b))
>                          cs = map (\x -> if any (\y -> (cfst y == fst x) && (csnd y == snd x)) (bsnd b) && (r == x)
>                                      then "[%]"
>                                      else if any (\y -> (cfst y == fst x) && (csnd y == snd x)) (bsnd b) 
>                                           then "[.]"
>                                      else if r == x
>                                           then "[o]" 
>                                      else "[ ]") (indices (rectSquareGrid (fst (bfst b)) (snd (bfst b))))
>                          ls = splitEvery (snd (bfst b)) cs
>                          s = reverse (lines (concat ls))
>                      r <- return s
>                      mapM putStrLn r
>                      putStrLn ""
>                      return ()


The very first thing we need to work out is movement around the board.
A movement is an action performed on Rob - none of the other pieces move.
Rob's movement does not affect any of the state on the board other than 
Rob.

Checking whether a move is out of bounds would also require returning a
reward, so we will leave checking to another function.

> moveRob      :: Rob -> Dir -> Rob
> moveRob (Rob (x, y)) d  = case d of
>                   U -> Rob (x+1, y)
>                   D -> Rob (x-1, y)
>                   R -> Rob (x, y+1)
>                   L -> Rob (x, y-1)

It would be nice to be able to see this happening on the board, so we will
need to write a function to visualize the board.

> move          :: Dir -> Board -> Board
> move dir (Board (dims, cans, rob)) = Board (dims, cans, (moveRob rob dir))