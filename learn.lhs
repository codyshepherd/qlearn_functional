
> import Board

The very first thing we need to work out is movement around the board.
A movement is an action performed on Rob - none of the other pieces move.
Rob's movement does not affect any of the state on the board other than 
Rob.

Checking whether a move is out of bounds would also require returning a
reward, so we will leave checking to another function.

> move      :: Rob -> Dir -> Rob
> move (Rob (x, y)) d  = case d of
>                   U -> Rob (x, y+1)
>                   D -> Rob (x, y-1)
>                   R -> Rob (x+1, y)
>                   L -> Rob (x-1, y)