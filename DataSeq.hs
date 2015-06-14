module DataSeq where

import Data.Sequence as S


--
-- Basic construction, append / prepend, concat etc
--

s0 :: Seq Int
s0 = fromList [1,2,3]

-- append
s1 = s0 |> 4


-- prepend
s2 = 0 <| s1

-- concat
s3 = s2 >< s2

-- replicate
s4 = S.replicate 10 42      -- handy for initialisation



--
-- operations on elements of Sequence
--

-- view (peek at L and R end of seq)
v1l = viewl s1
v1r = viewr s1

-- index (get the i'th element of the seq, starting from 0)
i0 = index s1 0 -- 1
i1 = index s1 1 -- 2
i2 = index s1 2 -- 3

--
