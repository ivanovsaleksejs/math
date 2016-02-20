{-# LANGUAGE PostfixOperators  #-}

-- Bernoulli formula efficiently calculates the probability
-- of k successful outcomes in n equal trials with possible results "true" of "false",
-- 
--
-- Example: a box with 20 white and 10 black balls, student does 4 tries
-- picking a ball, each time putting the ball back
-- What is a probability that he picks the white ball 2 times of 4?

module Main where

(!) = product . enumFromTo 1

c a b = (b !) / ((a !) * ((b - a) !))

bernoulli success (good, all) trials = c success trials * (p ** success) * (q ** (trials - success))
    where
        p = good / all
        q = 1 - p

main = print $ bernoulli 2 (20,30) 4
