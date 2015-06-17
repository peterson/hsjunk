module Lines where

import Control.Monad (forever)
import Data.Char (isLower, toLower, ord, chr)


--
-- main
--

main :: IO ()
-- ^---------- main is the name of the function
--   ^-------- :: is read as "has type" or "of type"
--      ^----- the IO 'monad' .. basically "IO stuff". (IO is a type constructor).
--         ^-- () is pronounced 'unit' ... basically like 'void' in other languages.
--
-- So, 'main :: IO ()' is similar to a 'void main()' definition in a C-like language.
--
-- Let's go the other way ... if we were to write an equivalent definition for the
-- C-like function 'void main()' in Haskell, it might look like this:
--
-- 'main :: void'
--
-- ... but there is no natural 'place' for the IO 'thing' in this definition.
--
-- However, if we think of 'IO' as something like a "hint" to a compiler (or
-- interpreter) that "IO stuff" was going to occur in the main function, we might
-- imagine using a language extension such as a 'pragma' or 'annotation', e.g.
--
-- // Java-like
-- @IO
-- void main() {...}
--
-- or
--
-- // C-like
-- #pragma io(main)   // using a typical lower-case pragma style i.e. IO -> io
-- void main() {...}
--
-- We can then "read" the above as .. "here's a main function that returns void,
-- and does some IO stuff".
--
-- Similarly, we can read 'main :: IO ()' as ... "here's a main function that
-- returns unit, and does some IO stuff".
--


-- canonical main
--
main =
  putStrLn "Hello, world"

--
-- run once version

-- main = do
--   l <- getLine
--   putStrLn (transform l)
--   return ()


--
-- run 'forever' version

-- main =
--   forever $ do
--     l <- getLine
--     putStrLn $ transform l
--     return ()


--
-- 'de-sugared' version of above

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
echo :: String -> String
echo s = s


--
-- rot13 rotation (yay!)
--

-- first, some helper functions ...

--
-- int to chr ... 0 -> 'a', 25 -> 'z'
int2chr :: Int -> Char
int2chr n = chr(ord 'a' + n)

--
-- chr to int ... 'a' -> 0, 'z' -> 25
chr2int :: Char -> Int
chr2int c = ord c - ord 'a'

--
-- shift a character by 'n' places
shift :: Int -> Char -> Char
shift n c | isLower c = int2chr((chr2int c + n) `mod` 26)
          | otherwise = c

--
-- generic rotation - rotate a string by 'n' places
rot :: Int -> String -> String
rot n [] = []       -- pattern match on []
rot n (x:xs) =      -- pattern match on [..]
  shift n x : rot n xs
--          ^ ... note ':' is the list constructor (we are building a new list)
--                                                 ... recursively!

--
-- ... alternatively we could use 'map'
-- rot n xs = map (shift n) xs
-- rot n = map (shift n)
-- rot = map . shift

--
-- ... or, more generally, 'fmap' (which is the same as 'map' for lists)
-- rot n xs = fmap (shift n) xs
-- rot n = fmap (shift n)
-- rot = fmap . shift


--
-- now we can define rot13! (note: partial application)
rot13 :: String -> String
rot13 = rot 13
