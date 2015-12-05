countParens :: String -> Int
countParens xs = foldr iter 0 xs
  where
    --iter :: Char -> Int -> Int
    iter '(' = (+1)
    iter ')' = (subtract 1)
    iter _ = id

findBasement :: String -> Int
findBasement xs = iter xs 0 1
  where
    iter [] _ _ = -1
    iter (y:ys) a b = if a == -1 then b-1 else iter ys (newFloor y a) b+1
      where newFloor '(' = (+1)
            newFloor ')' = (subtract 1)
            newFloor _ = id

--main = interact ((++ "\n") . show . countParens)
main = interact ((++ "\n") . show . findBasement)
