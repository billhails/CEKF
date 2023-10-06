import System.IO
import qualified Data.Set as Set

main = do
    handle <- openFile "/usr/share/dict/words" ReadMode
    contents <- hGetContents handle
    let dict = Set.fromList (words contents)
        revWords = Set.filter (\ word -> Set.member (reverse word) dict) dict
    mapM_ print (map addRev (Set.elems revWords))

addRev word
    | word == (reverse word) = word
    | otherwise = word ++ " " ++ (reverse word)
