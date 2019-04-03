import Data.List
import Data.List.Split
import Data.Complex

main :: IO()
main = do
    putStr "Enter the amplitudes of the initial state: "
    a <- getLine
    let s = state a
    if (isNormalized s) == False
      then putStr "Error: The vector is not normalized!"
      else do
      o <- getChar
      operate o s

---------- Functions ---------
-- Operator
operate :: Char -> (Complex Double, Complex Double) -> String
operate c s
    | c == 'H' = current $ hadamard $ s
    | c == 'X' = current $ xGate $ s
    | c == 'Y' = current $ yGate $ s
    | c == 'Z' = current $ zGate $ s
    | c == 'M' = measure s
    | otherwise = "Enter a valid operation!(H,X,Y,Z,M)"

-- Printer
current :: (Complex Double, Complex Double) -> String
current a = "Current State: ((" ++ r0 ++ ") + (" ++ i0 ++ ")i)|0> + ((" ++ r1 ++ ") + (" ++ i1 ++ ")i)|1>"
          where r0 = show $ realPart $ fst a
                i0 = show $ imagPart $ fst a
                r1 = show $ realPart $ snd a
                i1 = show $ imagPart $ snd a

-- State
state :: String -> (Complex Double, Complex Double)
state a = (getComplex $ head $ b, getComplex $ last $ b)
        where b = map cParse (splitOn "," a)

-- Parser
cParse :: String -> (Double,Double)
cParse a
    | length b == 2      = (b1,b2)
    | 'i' `elem` head b  = (0,b2)
    | otherwise          = (b1,0)
    where b = map (delete '+') (split(startsWithOneOf['+','-']) a)
          b1 = read $ head b
          bi = init $ last b
          b2 = if bi == "" then 1 else if bi == "-" then -1 else read bi

-- Parsed to Complex
getComplex :: (RealFloat a) => (a,a) -> Complex a
getComplex (a,b) = (a:+b)

-- Matrix multiplication
tform :: Num a => (a,a) -> ((a,a),(a,a)) -> (a,a)
tform (a,b) ((k,l),(m,n)) = (a*k + b*m, a*l + b*n)

-- Checks if the state is normalized
isNormalized :: (Complex Double,Complex Double) -> Bool
isNormalized a = if m >= 0.95 && m <= 1.05 then True else False
                where m = magnitude(fst a)^2 + magnitude(snd a)^2


-------- Gates for 1 qubit ------
-- Hadamard Gate

hadamard :: (RealFloat a) => (Complex a,Complex a) -> (Complex a,Complex a)
hadamard a = tform a ((1/(sqrt 2),1/(sqrt 2)),(1/(sqrt 2),-1/(sqrt 2)))

-- X-Gate
xGate :: (RealFloat a) => (Complex a,Complex a) -> (Complex a,Complex a)
xGate a = tform a ((0,1),(1,0))

-- Y-Gate
yGate :: (RealFloat a) => (Complex a,Complex a) -> (Complex a,Complex a)
yGate a = tform a ((0,0:+(-1)),(0:+1,0))

-- Z-Gate
zGate :: (RealFloat a) => (Complex a,Complex a) -> (Complex a,Complex a)
zGate a = tform a ((1,0),(0,-1))

-- Measurement
measure :: (Complex Double,Complex Double) -> String
measure a = "The outcome is |0> with probability " ++ p0 ++ "; |1> with probability " ++ p1 ++ "."
            where p0 = show $ magnitude(fst a)
                  p1 = show $ magnitude(snd a)

