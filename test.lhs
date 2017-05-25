
> import Learning
> import qualified Data.Map as Map


> main = do let s = [E, C, E, C, C]
>               k = Key (stateKey s)
>               v = Values [0,0,0,0,0]
>               qt = Map.fromList [(k,v)]
>               qt' = Map.insert (Key (stateKey [E, E, E, E, C])) (Values [1, 0,0,0,0]) qt
>           print qt
>           print qt'