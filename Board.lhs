Cody Shepherd
Q-Learning with Haskell

> module Board where
> import Math.Geometry.Grid
> import Math.Geometry.Grid.Square

The Rob type is an instance of Rob's location on the board.

> data Rob = Rob (Int, Int)
>               deriving (Show, Eq)

> rfst      :: Rob -> Int
> rfst (Rob (a, b)) = a

> rsnd      :: Rob -> Int
> rsnd (Rob (a, b)) = b

The first thing Rob has to do is move around a board. 
It seems like we don't need to store all the empty locations... we can just
track where there are cans. A can, like Rob, is defined by its location.

A location will occur in the context of the +,+ quadrant of a cartesian plane,
so, (x, y) where both are positive, x representing the horizontal and y the vertical.

> data Can = Can (Int, Int)
>            deriving (Show, Eq)

> cfst       :: Can -> Int
> cfst (Can (a, b)) = a

> csnd          :: Can -> Int
> csnd (Can (a, b)) = b


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