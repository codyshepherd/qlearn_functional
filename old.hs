import System.Random
import Data.List
import Data.List.Utils
import Data.List.Extras.Argmax
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import Debug.Trace

n_global = 5000
m_global = 200
eta = 0.2 :: Float
gamma = 0.9 :: Float

data KeyNotFoundException = KeyNotFoundException deriving (Show)
data CanNotFoundException = CanNotFoundException deriving (Show)

instance Exception KeyNotFoundException
instance Exception CanNotFoundException

data Cell = Empty | Wall | Can | ERob | CRob deriving (Show, Eq)

data Grid = Grid{   cells   :: [[Cell]]
                ,   loc     :: (Int, Int)
                ,   reward  :: Float
                ,   step    :: Int
                ,   qtable  :: Map String [Float]} deriving (Show, Eq)

data Act = U | D | L | R | P deriving (Show, Eq)

data State = State{ north :: Cell
                  , south :: Cell
                  , east  :: Cell
                  , west  :: Cell
                  , here  :: Cell
                  , key   :: String} deriving (Show, Eq)

actToInt :: Act -> Int
actToInt a = case a of
                    U -> 0
                    D -> 1
                    L -> 2
                    R -> 3
                    P -> 4

getCell :: Int -> Cell
getCell n = case n `mod` 2 of
            0 -> Can
            _ -> Empty

getAction :: Int -> Act
getAction n =  case n `mod` 5 of
                0 -> U
                1 -> D
                2 -> L
                3 -> R
                4 -> P

{-
getBool :: Int -> Bool
getBool n = case n `mod` 2 of
                0 -> True
                1 -> False
-}

randomCell  :: IO Cell
randomCell = do n <- randomRIO(1,2)
                return (getCell n)

infixr `times`
times       :: Int -> IO a -> IO [a]
n `times` action = sequence (replicate n action)

randomField :: Int -> IO [[Cell]]
randomField d = d `times` d `times` randomCell

horizWall :: Int -> [Cell]
horizWall n = [Wall | i<-[1..n]]

addWalls :: Int -> [[Cell]] -> [[Cell]]
addWalls n cs = let l =  horizWall n : [[Wall] ++ c ++ [Wall] | c <- cs] in l ++ [horizWall n]

lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

randomLoc :: Int -> IO (Int, Int)
randomLoc n = do i <- randomRIO(1,n)
                 j <- randomRIO(1,n)
                 return (i,j)

randomAct :: IO Act
randomAct = do n <- randomRIO(0,99)
               return (getAction n)

randomBool :: Float -> IO Bool
randomBool p = do n <- randomRIO(1,100)
                  return (if n > (p*100) then True else False)

randomActions :: Int -> Int -> IO [Act]
randomActions n m = (n*m) `times` randomAct

{-
isActionRandom :: Float -> Int -> Int -> Int -> IO [Bool]
isActionRandom eps n m step = mapM randomBool [floor (eps-(0.01* fromIntegral y) * 100.0)
                                | x <- [1..(n*m)]
                                , y <- [x `div` (m*step)]]
-}

isActionRandom :: Int -> Float -> IO [Bool]
isActionRandom 1000000 f = return([])
isActionRandom i f = do n <- randomRIO(0.01,0.99)
                        let b = if n < f then True else False
                            n_eps = (0.01 * fromIntegral (i `div` 10000))
                        bs <- isActionRandom (i+1) (1-n_eps)
                        --return (b: isActionRandom (i+1) (1 - n_eps))
                        return (b : bs)


addRob :: (Int, Int) -> [[Cell]] -> [[Cell]]
addRob loc cs = let r = if cs !! (snd loc) !! (fst loc) == Empty then ERob else CRob
                    i = fst loc
                    j = snd loc
                    in take j cs ++ [take i (cs !! j) ++ [r] ++ lastN' (9-i) (cs !! j)] ++ lastN' (9-j) cs

removeRob :: [[Cell]] -> [[Cell]]
removeRob cs = let gs = [replace [ERob] [Empty] c | c <- cs] in [replace [CRob] [Can] g | g <- gs]

removeCan :: (Int, Int) -> [[Cell]] -> [[Cell]]
removeCan loc cs = if cs !! (snd loc) !! (fst loc) == CRob
                    then
                       let  i = fst loc
                            j = snd loc
                        in take j cs ++ [take i (cs !! j) ++ [ERob] ++ lastN' (9-i) (cs !! j)] ++ lastN' (9-j) cs
                    else throw CanNotFoundException

isCan :: (Int, Int) -> [[Cell]] -> Bool
isCan loc cs = let  i = fst loc
                    j = snd loc
                    in if cs !! j !! i == Can || cs !! j !! i == CRob then True else False

letterOf :: Cell -> String
letterOf Empty = "[ ]"
letterOf Wall = " = "
letterOf Can = "[c]"
letterOf ERob = "[o]"
letterOf CRob = "[8]"

listValues :: [[Cell]] -> [[String]]
listValues xs = map (map letterOf) xs

printField :: [[Cell]] -> IO ()
printField xs = mapM_ putStrLn [ intercalate "" a | a<-listValues xs]

getKey :: [Cell] -> String
getKey [] = ""
getKey (c:cs) =
    case c of
        Empty -> '0':getKey cs
        Wall -> '1':getKey cs
        Can -> '2':getKey cs
        ERob -> '3':getKey cs
        CRob -> '4':getKey cs

move :: Act -> Grid -> Grid
move dir grd =   let    rw = checkMove dir grd
                        qt = qtable grd
                        i = fst (loc grd)
                        j = snd (loc grd)
                        st = step grd
                        in case dir of
                            U -> let lc = if j > 1
                                            then (i, j-1)
                                            else (i, j) in Grid{ cells = addRob lc (removeRob (cells grd))
                                                               , loc=lc, reward=rw, step=st+1, qtable=qt}
                            D -> let lc = if j < 8
                                            then (i, j+1)
                                            else (i, j) in Grid{ cells = addRob lc (removeRob (cells grd))
                                                               , loc=lc, reward=rw, step=st+1, qtable=qt}
                            L -> let lc = if i > 1
                                            then (i-1, j)
                                            else (i, j) in Grid{ cells = addRob lc (removeRob (cells grd))
                                                               , loc=lc, reward=rw, step=st+1, qtable=qt}
                            R -> let lc = if i < 8
                                            then (i+1, j)
                                            else (i, j) in Grid{ cells = addRob lc (removeRob (cells grd))
                                                               , loc=lc, reward=rw, step=st+1, qtable=qt}
                            P -> let lc = loc grd in if isCan lc (cells grd) == True
                                                        then Grid{ cells = removeCan lc (cells grd)
                                                                 , loc=lc, reward=rw, step=st+1, qtable=qt}
                                                        else Grid{ cells = cells grd
                                                                 , loc = lc
                                                                 , reward = rw
                                                                 , step = st+1
                                                                 , qtable=qt }


checkMove :: Act -> Grid -> Float
checkMove dir grd = let s = getState grd in case dir of
                                                U -> case north s of
                                                        Wall -> -5.0
                                                        _ -> 0.0
                                                D -> case south s of
                                                        Wall -> -5.0
                                                        _ -> 0.0
                                                L -> case west s of
                                                        Wall -> -5.0
                                                        _ -> 0.0
                                                R -> case east s of
                                                        Wall -> -5.0
                                                        _ -> 0.0
                                                P -> case here s of
                                                        CRob -> 10.0
                                                        _ -> -1.0


getState :: Grid -> State
getState grd = let  i = fst (loc grd)
                    j = snd (loc grd)
                    in let  n = cells grd !! (j-1)  !! i
                            s = cells grd !! (j+1)  !! i
                            e = cells grd !! j      !! (i+1)
                            w = cells grd !! j      !! (i-1)
                            h = cells grd !! j      !! i
                            k = getKey [n,s,e,w,h]
                            in State{ north = n
                                    , south = s
                                    , east = e
                                    , west = w
                                    , here = h
                                    , key = k}

newQTable :: Map String [Float]
newQTable = let strings = [ [n] ++ [s] ++ [e] ++ [w] ++ [h] | n <- ['0'..'2'], s <- ['0'..'2']
                                                  , e <- ['0'..'2'], w <- ['0'..'2'], h <- ['0'..'4']]
                in Map.fromList (map makePair strings)
                    where makePair x = (x, [0.0,0.0,0.0,0.0,0.0])

getNewAction :: [Act] -> [Bool] -> Grid -> Act
getNewAction randActs isRandom grd = let i = step grd
                                        in if isRandom !! i == True
                                           then randActs !! i
                                           else getBestAction grd

maxI :: [Float] -> Int
maxI xs = let (f, i) = maximumBy (comparing fst) (zip xs [0..]) in i

getBestAction :: Grid -> Act
getBestAction grd = let k = key (getState grd)
                        l = Map.lookup k (qtable grd)
                        in case l of
                            Just n -> getAction (maxI n)
                            Nothing -> throw KeyNotFoundException

repl :: Int -> Float -> [Float] -> [Float]
repl i new lst = [if j == i then new else x | x<-lst, j<-[0..((length lst)-1)]]

computeQ :: String -> String -> Int -> Float -> Map String [Float] -> Float
computeQ s s' i r qt = let  q = (Map.lookup s qt)
                            --q' = Map.lookup s'
                            in case q of
                                Just a -> let y = a !! i
                                              q' = Map.lookup s' qt
                                              in case q' of
                                                Just z -> y + (eta * (r + gamma*(argmax (\x -> x-0.0) z) - y))
                                                Nothing -> throw KeyNotFoundException
                                Nothing -> throw KeyNotFoundException

learn :: State -> State -> Act -> Grid -> Grid
learn s s' a grd = let qt = qtable grd
                       rw = reward grd
                       sk = key s
                       sk' = key s'
                       ind = actToInt a
                       lst = Map.lookup sk qt
                       in case lst of
                            Just y -> let newlist = repl ind (computeQ sk sk' ind rw qt) y
                                        in Grid{ cells   = cells grd
                                               , loc     = loc grd
                                               , reward  = reward grd
                                               , step    = step grd
                                               , qtable  = Map.insert sk newlist (Map.delete sk qt)}
                            Nothing -> throw KeyNotFoundException

execEpisode :: [Act] -> [Bool] -> Int -> Grid -> Grid
execEpisode rands isRands i grd = let s = getState grd
                                      act = getNewAction rands isRands grd
                                      grd' = move act grd
                                      s' = getState grd'
                                      in if i == m_global
                                         then learn s s' act grd'
                                         else execEpisode rands isRands (i+1) (learn s s' act grd')

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

execProg :: [Act] -> [Bool] -> Int -> Int -> Grid -> IO ()
execProg rands isRands episode stp initgrid =
    if episode == n_global
    then return ()
    else do
        let final = execEpisode rands isRands stp initgrid
            qt = qtable final
        print ("End of episode" ++ show episode)
        printField (cells final)
        --print qt
        f <- randomField 8
        l <- randomLoc 8
        let randoms = drop m_global rands
            isRandoms = drop m_global isRands
        execProg randoms isRandoms (episode+1) stp Grid{ cells = addRob l (addWalls 10 f)
                                                       , loc = l
                                                       , reward = 0
                                                       , step = 0
                                                       , qtable = qt}


main = do
    f <- randomField 8
    l <- randomLoc 8
    randoms <- randomActions n_global m_global
    --isRandoms <- isActionRandom 1 n_global m_global 50
    isRandoms <- isActionRandom 0 1.0
    let
        ff = addWalls 10 f
        table = newQTable
        grid = Grid{cells = addRob l ff
                   ,loc = l
                   ,reward = 0
                   ,step = 0
                   ,qtable = table}
    execProg randoms isRandoms (n_global-201) 0 grid
    --print (qtable grid)
