Cody Shepherd
Q-Learning with Haskell

> module Board where

The Rob type is an instance of Rob's location on the board.

> data Rob = Rob (Int, Int)
>               deriving (Show)

The first thing Rob has to do is move around a board. 
It seems like we don't need to store all the empty locations... we can just
track where there are cans. A can, like Rob, is defined by its location.

A location will occur in the context of the +,+ quadrant of a cartesian plane,
so, (x, y) where both are positive, x representing the horizontal and y the vertical.

> data Can = Can (Int, Int)
>            deriving (Show, Eq)

For the board, we will have to define limits, but otherwise we don't need to store 
empty cells.

ATM we define Board as a 3-tuple with its height and width limits, pluts a list
of cans within it: ((MAXHEIGHT, MAXWIDTH), Cans)

> data Board = Board ((Int, Int), [Can])
>               deriving (Eq, Show)

If Rob is going to move, we would like some way of constraining his possible movements
to the cardinal directions. We'll define a direction in the order of "nsew", though it 
doesn't make any difference.

> data Dir =  U
>           | D
>           | R
>           | L
>           deriving (Show, Eq)