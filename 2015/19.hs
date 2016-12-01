import Data.List
import Data.String.Utils
import Data.Set (fromList, toList)

replacements :: String -> String -> String -> [String]
replacements from to [] = []
replacements from to str@(x:xs) | from `isPrefixOf` str = (to ++ drop (length from) str) : rest
                                | otherwise = rest
                                where rest = map (x:) $ replacements from to xs

allReplacements :: [(String, String)] -> String -> [String]
allReplacements pairs str = concatMap (\(from, to) -> replacements from to str) pairs

readSubstitutions :: [String] -> [(String, String)]
readSubstitutions ls =
    let
        (_:_:ls') = reverse ls
        toPair xs = (from, to)
            where (from:to:_) = split " => " xs
     in
        map toPair ls'

readMolecule :: [String] -> String
readMolecule = last

uniq :: Ord a => [a] -> [a]
uniq = toList . fromList

reverseSubsts:: [(String, String)] -> [(String, String)]
reverseSubsts = map (\(a, b) -> (b, a))

chainReplacements :: [(String, String)] -> [String] -> [[String]]
chainReplacements substs seed =
    let a = uniq $ concatMap (allReplacements substs) seed
     in a : chainReplacements substs a

stepsTo :: [(String, String)] -> String -> String -> Int
stepsTo substs start finish = (+1) . length . takeWhile (not . (finish `elem`)) $ chainReplacements substs [start]

main = do
    input <- getContents
    let ls = lines input
        substs = readSubstitutions ls
        mol = readMolecule ls
        result = uniq $ allReplacements substs mol
    print $ length result
    -- print $ stepsTo (reverseSubsts substs) mol "e"
