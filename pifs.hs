-- The proof of concept of so called pi-fs
--
-- The idea is: as pi constant is transcendental number
-- it should have any pattern of decimal digits inside.
-- So, if one uses decimal system to encode files,
-- he would be able to encode any possible file 
-- just by knowing offset and length.
--
-- This code proves that it's possible to decode pattern when only 
-- offset data and length are provided. In other words, it is
-- possible to restore generation of pi digits from
-- some specific point.
--
-- Offset data contains extremely huge integer numbers
-- which makes this implementation of idea highly impractical.
--
-- It is possible to use only offset number and length to
-- generate a sequence (functions encodeByOffset and decodeByOffset),
-- but it requires to generate all elements before,
-- which makes decoding very time consuming.

module Main where

type Pentaple t = (t,t,t,t,t)

-- Function that generates decimal digits of pi using Gosper's series.
-- Function can restore generation from specific offset
-- if 5 parameters are provided
step :: Integral t => Pentaple t -> Pentaple t
step (_, q, r, t, i) = (
        y,
        10 * q * i * (f 2 (-1)),
        10 * u * (q * (f 5 (-2)) + r - y * t),
        t * u,
        i + 1
    )
    where
        y = (q * (f 27 (-12)) + 5 * r) `div` (5 * t)
        u = 3 * (f' 1) * (f' 2)
        f  = (+) . (i *)
        f' = f 3

-- First element of 5-tuple
fst5 :: Pentaple t -> t
fst5 (a, _, _, _, _) = a

-- Fifth element of 5-tuple
fifth :: Pentaple t -> t
fifth (_, _, _, _, a) = a

-- Init values for pi sequence
initPi :: Integral t => Pentaple t
initPi = (3, 1, 180, 60, 2)

-- Infinite lazy list of sets
lazySet :: Integral t => Pentaple t -> [Pentaple t]
lazySet set = gen
    where
        gen = set : scanl (const . step) (step set) gen

-- Finds an element in pi sequence that would point to an offset
-- from which the pattern is encoded in pi
find :: Integral t => Pentaple t -> [t] -> Pentaple t
find set needle
    | pattern == needle = set
    | otherwise         = find (step set) needle
    where
        setGroup = take (length needle) $ lazySet set
        pattern  = map fst5 setGroup

-- Finds an element by specific offset
findByOffset :: Integral t => Pentaple t -> (Int, t) -> [t]
findByOffset set (n, offset)
    | offset == fifth set = take n $ map fst5 $ lazySet set
    | otherwise           = findByOffset (step set) (n, offset)


-- Encoding function that finds an element in pi sequeence with additional
-- parameters which would allow to restore pi sequence generation
-- from where it points without generating previous elements.
-- It calls find function with init values of pi sequence.
-- encodeByOffset is encode version that returns only offset number and length
encode         :: [Integer] -> (Int, Pentaple Integer)
encodeByOffset :: [Integer] -> (Int, Integer)
(encode, encodeByOffset) = (doEncode id, doEncode fifth)
    where
        doEncode f pattern = (length pattern, f $ find initPi pattern)

-- Decoding function that generates n elements using element returned by
-- encode function as an offset element
decode :: Integral t => (Int, Pentaple t) -> [t]
decode (length, set) =  take length $ map fst5 $lazySet set

-- Decode version that uses only offset number and length
decodeByOffset :: (Int, Integer) -> [Integer]
decodeByOffset = findByOffset initPi 

main :: IO ()
main = do
--    print $ encode  [1, 2, 3, 4]
--    print $ encodeByOffset  [1, 2, 3, 4]
--    uncomment lines above to output a structure of the offset element
    print $ decode $ encode [1, 2, 3, 4]
    print $ decodeByOffset $ encodeByOffset [1, 2, 3, 4]
