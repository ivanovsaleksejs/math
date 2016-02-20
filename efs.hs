encode pattern = (len, check len $ map a001113 [1..len])
    where
        len   = length pattern
        check index lst 
            | pattern == lst = index - len + 1
            | otherwise      = check next $ tail lst ++ [a001113 next]
            where
                next = index + 1

a001113 n = a001113_list !! (n-1)

a001113_list = eStream (1, 0, 1) [(n, a * d, d) | (n, d, a) <- map (\k -> (1, k, 1)) [1..]]

eStream z xs'@(x:xs)
    | lb /= approx z 2 = eStream (mult z x) xs
    | otherwise        = lb : eStream (mult (10, -10 * lb, 1) z) xs'
        where
            lb                       = approx z 1
            approx (a, b, c) n       = div (a * n + b) c
            mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f) 

decode (l, n) = take l $ drop (n-1) a001113_list

main = do
    print $ encode [1,2,3,4]
    print $ decode $ encode [1,2,3,4]
