module Lines where

import Control.Monad (forever)
import Data.Char (isLower, toLower, ord, chr)


--
-- main
--

main :: IO ()

-- canonical main
--
main =
  putStrLn "Hello, world"


-- run once version
--
-- main = do
--   l <- getLine
--   putStrLn (transform l)
--   return ()


--
-- run 'forever' version
--
-- main =
--   forever $ do
--     l <- getLine
--     putStrLn $ transform l
--     return ()


-- 'de-sugared' version of above
--
-- main =
--   forever $
--     getLine >>= \l -> ( (putStrLn $ transform l) >> return () )


-- Bonus point: showing GHC 'core' (de-sugared and optimised syntax)
-- $ ghc -ddump-ds -dsuppress-uniques Lines.hs


--
-- transform takes a string, and gives one back
transform :: String -> String
transform = echo
-- transform = id
-- transform = reverse
-- transform = reverse . reverse
-- transform = rot13
-- transform = rot13 . rot13


--
-- echo (.. gives you back the string)
--
echo :: String -> String
echo s = s


--
-- rot13 rotation (yay!)
--

-- first, some helper functions ...
-- int to chr ... 0 -> 'a', 25 -> 'z'
int2chr :: Int -> Char
int2chr n = chr(ord 'a' + n)

-- chr to int ... 'a' -> 0, 'z' -> 25
chr2int :: Char -> Int
chr2int c = ord c - ord 'a'

-- shift a character by 'n' places
shift :: Int -> Char -> Char
shift n c | isLower c = int2chr((chr2int c + n) `mod` 26)
          | otherwise = c

-- generic rotation - rotate a string by 'n' places
rot :: Int -> String -> String
rot n [] = []       -- pattern match on []
rot n (x:xs) =      -- pattern match on [..]
  shift n x : rot n xs
--          ^ ... note ':' is the list constructor (we are building a new list)
--                                                 ... recursively!

-- rot13 (note: partial application)
rot13 :: String -> String
rot13 = rot 13
