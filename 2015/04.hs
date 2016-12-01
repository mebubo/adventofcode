import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 (pack)

input = "iwrupvqb"

appendInt :: String -> Int -> String
appendInt s x = s ++ show x

md5String :: String -> String
md5String = show . md5 . pack

startsWithNZeroes :: Int -> String -> Bool
startsWithNZeroes n = all (=='0') . take n

allHashes =  \i -> map (md5String . appendInt i) [0..]
upToFirstMatch = \x -> takeWhile (not . startsWithNZeroes x)

main :: IO ()
--main = print (length $ upToFirstMatch 5 $ allHashes input)
main = print (length $ upToFirstMatch 6 $ allHashes input)

