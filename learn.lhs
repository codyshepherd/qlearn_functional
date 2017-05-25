
> import Board

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