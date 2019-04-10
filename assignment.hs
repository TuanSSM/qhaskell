import Data.List
import Data.List.Split
import Data.Complex
import System.IO

main :: IO()
main = do
    hSetBuffering stdin NoBuffering
    putStrLn "Enter the amplitudes of the initial state: "
    amplitude <- getLine
    let s = state amplitude
    if not (isNormalized s)
      then putStrLn "Error: The vector is not normalized!"
      else do
        let currentState = current s
        putStrLn currentState
        putStrLn "Enter the transformation: "
        operator s

-- === Functions === --
-- Operator
operator :: (Complex Float, Complex Float) -> IO()
operator s = do
    o <- getChar
    putStrLn ""
    if o `notElem` "HXYZM" then putStrLn "Error: Enter a valid operation![H,X,Y,Z,M]"
      else if o == 'M'
        then do putStrLn (measure s)
                putStrLn "Do you want to continue?[Y/N]"
                c <- getChar
                putStrLn ""
                if c == 'n' || c == 'N' then return ()
                  else do main
      else do
        let newState = operate o s
        putStrLn (current newState)
        putStrLn "Enter the transformation: "
        operator newState

operate :: Char -> (Complex Float, Complex Float) -> (Complex Float, Complex Float)
operate c
    | c == 'H' = tform hGate
    | c == 'X' = tform xGate
    | c == 'Y' = tform yGate
    | c == 'Z' = tform zGate
    where hGate = ((1/(sqrt 2),1/(sqrt 2)),(1/(sqrt 2),-1/(sqrt 2)))
          xGate = ((0,1),(1,0))
          yGate = ((0,0:+(-1)),(0:+1,0))
          zGate = ((1,0),(0,-1))

tform :: Num a => ((a,a),(a,a)) -> (a,a) -> (a,a)
tform ((k,l),(m,n)) (a,b) = (a*k + b*m, a*l + b*n)

-- Printer
measure :: (Complex Float,Complex Float) -> String
measure a = "The outcome is |0> with probability " ++ p0 ++ "; |1> with probability " ++ p1 ++ "."
            where p0 = show $ fst a * (conjugate $ fst a)
                  p1 = show $ snd a * (conjugate $ snd a)

current :: (Complex Float, Complex Float) -> String
current a = "Current State: ((" ++ r0 ++ ") + (" ++ i0 ++ ")i)|0> + ((" ++ r1 ++ ") + (" ++ i1 ++ ")i)|1>"
          where r0 = show $ realPart $ fst a
                i0 = show $ imagPart $ fst a
                r1 = show $ realPart $ snd a
                i1 = show $ imagPart $ snd a

state :: String -> (Complex Float, Complex Float)
state a = ((fst $ head b):+(snd $ head b),(fst $ last b):+(snd $ last b))
        where b = map parse (splitOn "," a)

-- Parser (can't read strings whithout comma, use "i,0" instead of "i"
parse :: String -> (Float,Float)
parse a
    | length b == 2      = (b1,b2)
    | 'i' `elem` head b  = (0,b2)
    | otherwise          = (b1,0)
    where b = map (delete '+') (split(startsWithOneOf['+','-']) a)
          b1 = read $ head b
          bi = init $ last b
          b2 = if bi == "" then 1 else if bi == "-" then -1 else read bi

-- Checks if the state is normalized
isNormalized :: (Complex Float,Complex Float) -> Bool
isNormalized a = if m >= 0.95 && m <= 1.05 then True else False
                where m = magnitude(fst a)^2 + magnitude(snd a)^2
