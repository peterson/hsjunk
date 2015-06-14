module Palindrome where

--
-- Some useful palindromes ...

otto = "otto"
otto2 = concatMap (replicate 2) otto
otto3 = concatMap (replicate 3) otto

-- ... and a helper function for the mix!
-- replicate each char in 's' n-times and flatten the result

nrep n s = concatMap (replicate n) s


--
--
-- Okay, let's start with a straight-forward implementation ...

pal :: String -> Bool
pal s =
  s == s'
    where
      s' = reverse s


-- Next, let's rewrite this to use an anonymous function (or "lambda abstraction").
--
-- ... A WHAT?
--
-- A λ abstraction is the definition of an anonymous function that takes a value
-- (e.g. 'x') and binds it to a λ term (e.g. 't') --> λx.t
--
-- A λ abstraction doesn't 'call' (or 'apply') the function, it just sets it up.
--
-- λ-calculus allows us to say things like: λx.x+x
--
-- or, more readably, using "->" notation: λx -> x + x
--
-- In λ-calculus, the concept of function application is provided by a process of
-- substitution called "β-reduction". For example: ((λx.x+x) 2) --> (2+2) --> 4
--
-- Anyway .. enough for now!
--
--
-- Essentially we want a lambda expression (anonymous function) along the lines of:
--
-- λx -> x == reverse x
--
-- (Note the above line is not correct λ-calculus ... illustrative purposes only!)
--
-- (I think the correct form is: λs.λEQ.λREVERSE.(EQ (REVERSE s) s) .. or similar?)

pal2 :: String -> Bool
pal2 =
  \s ->
    s == reverse s


-- Next, let's re-work the function to use (==) in the _prefix_ position.
-- In Haskell, we need to (bracket) an infix operator when used in prefix
-- position ... so (a == b) becomes (==) a b

pal3 :: String -> Bool
pal3 =
  \s ->
    (==) s (reverse s)


-- Now, we might consider using the $ symbol to replace the ( ... ) brackets
pal4 :: String -> Bool
pal4 =
  \s ->
    (==) s $ reverse s


--
-- Now we're surely approaching the point(-free?) of no return!
--


-- It's time to go completely silly .. for just a bit!
--
-- 1. Consider for a moment a function (<*>) (which we'll call "sequential
-- application").
-- Q: What is the type signature of the '<*>' function?
--
-- :t (<*>)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--
-- YIKES!
--
--
-- 2. Now recall the function (==) which we've been using in all our examples.
-- Q: What is the type signature of (==) ?
--
-- :t (==)
-- (==) :: Eq a => a -> a -> Bool
--                 ^^^^^^^^^^^^^^
--
-- This makes sense, right? --> '==' takes an a, and another a, and gives a Bool.
--
--
-- 3. I wonder what happens if we try to "plug together" '<*>' and '==' ?
-- Q: Does this work? How would we know? ==> Use the types !
--
-- :t (<*>) (==)
-- (<*>) (==) :: Eq a => (a -> a) -> a -> Bool
--                       ^^^^^^^^   ^^^   ^^^^
--
-- Okay .. yes it does work ... But what does all this mean?
-- No we have something that takes a function (a -> a), and a thing (a), and
-- gives a Bool.
--
-- Recall that the function reverse :: [a] -> [a] (or [Char] -> [Char] when applied)
-- to a [Char] ... Hmm ... this might be useful!
--
--
-- 4. So, what happens when we "plug together" '<*>', '==' and the 'reverse' function?
-- Q: What is the type signature that results?
--
-- :t (<*>) (==) reverse
-- (<*>) (==) reverse :: Eq a => [a] -> Bool
--                               ^^^^^^^^^^^
--
-- Now _this_ looks promising .. We have a (partially applied) function that takes
-- an [a] (a list of something), and returns a Bool .. Um, [Char] -> Bool, perhaps?
--
-- We now have everything we need .. How did we get here? By following the types!
-- (... and some Hoogling!)

pal5 :: String -> Bool
pal5 =
  \s -> (<*>) (==) reverse s


--
-- Note that another effect of using '<*>' to define the function is that we're
-- now down to a single occurance of 's' in the function definition!
-- (All prior definitions had two, appearing on both sides of the (==) fn!)
--
-- So now, with 's' as the final argument to the function, we can choose to
-- drop it altogether, if we wish.
--
-- This is the (so-called) "point-free" style ... i.e. functions as compositions
-- of other functions, with no explicit reference to the arguments being passed.

pal6 :: String -> Bool
pal6 = (==) <*> reverse   -- we've moved <*> to infix position


--
-- Bonus point: the "Owl" ... ((.)$(.))
--
-- owl :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
--
-- where
--
-- owl = ((.)$(.))
--
-- (.) is function composition, and ($) is application, with the types:
--
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- --> which is simply f . g ... i.e. f . g (x) = f (g x)
--
-- ($) :: (a -> b) -> a -> b
-- i.e. take a function (a -> b) and apply it to a, to give b.
-- --> which is simply f ( a ) ... i.e. f $ a = f a
--

owl :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
owl = ((.)$(.)) -- of course, outer (...) purely for stylistic effect!


--
-- Also, here is owl', the pointful equivalent. (Note type signature is identical!)
--
-- owl' :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
-- f :: (a -> b -> c) --> i.e. a function that takes 2 arguments (.. caveat: partial application!)
-- g :: a  --> i.e. a bare value
-- h :: (a1 -> b) --> i.e. a function of 1 arg
-- k :: a1 --> i.e a bare value

owl' :: (a -> b -> c) -> a -> (a1 -> b) -> a1 -> c
owl' f g h k = f g (h k)

-- An example:
--
-- owl (==) 1 (1+) 0
--
-- reduces to: (==) 1 (1+0)
--             (==) 1 (1)
--             1 == 1
--             True



-- Of course, this now brings us to one final definition of our favourite
-- function, courtesy of the owl!

pal7 :: String -> Bool
pal7 =
  \s -> ((.)$(.)) (==) s reverse s

    -- hoot, hoot!
