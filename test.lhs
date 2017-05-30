
> module Main(main) where
> import Board
> import Learning
> import qualified Data.Map as Map


> main = do let b = Board ((8,8), [Can (2,3), Can (3,4), Can (3, 6)], Rob(3, 3))
>               b' = move U b
>               b'' = move R b
>           showBoard b
>           print (observe b)
>           showBoard b'
>           print (observe b')
>           showBoard b''
>           print (observe b'')


               k = Key (stateKey s)
               v = Values [0,0,0,0,0]
               qt = Map.fromList [(k,v)]
               qt' = Map.insert (Key (stateKey [E, E, E, E, C])) (Values [1, 0,0,0,0]) qt
           print qt'