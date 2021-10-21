import RainbowAssign
import System.Random
import qualified Data.Map as Map
import Data.Maybe

-- Provided parameters - ver.1
pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- number of hash/reduce ops applied to each chain
height = 1000           -- number of intial passwords
filename = "table.txt"  -- filename to store the table


-- Opposite of pwHash
-- i.e. maps a hash value to an arbitrary password
pwReduce :: Hash -> Passwd
pwReduce = reverse . (map toLetter) . take pwLength . toBaseN nLetters . fromEnum
  where toBaseN :: Int -> Int -> [Int]
        toBaseN n x = (x `mod` n) : toBaseN n (x `div` n)


-- Input:  Width of the table, and list of initial passwords
-- Output: A Map.Map that maps the final Hash values onto the initial Passwd values
rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable i pws = Map.fromList (zip (multiHashReduceAll i pws) pws)
  where multiHashReduceAll :: Int -> [Passwd] -> [Hash]
        multiHashReduceAll = map . multiHashReduce
        multiHashReduce :: Int -> Passwd -> Hash
        multiHashReduce 0 = pwHash
        multiHashReduce i = multiHashReduce (i-1) . pwReduce . pwHash


-- Input:  Rainbow table, width of table, hash value
-- Output: Maybe Passwd
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table width hash = hashIterateFind table width hash hash
  where hashIterateFind :: Map.Map Hash Passwd -> Int -> Hash -> Hash -> Maybe Passwd
        -- Keep reducing and hashing the given hash value for up to tableWidth times
        -- Until the value appears in the right-most column
        -- Then extract the initial password to start traversing
        hashIterateFind _ (-1) _ _ = Nothing
        hashIterateFind table i thisHash targetHash = case Map.lookup thisHash table of
          Nothing -> hashIterateFind table (i-1) (pwHash $ pwReduce $ thisHash) targetHash
          Just pw -> (case traverseChain table width targetHash pw of
            Nothing -> hashIterateFind table (i-1) (pwHash $ pwReduce $ thisHash) targetHash
            Just foundpw -> Just foundpw)
        traverseChain :: Map.Map Hash Passwd -> Int -> Hash -> Passwd -> Maybe Passwd
        -- Starting from initial Passwd,
        -- Traverse chain to find the Passwd that hashes to given Hash
        traverseChain table width targetHash pw
          | (width > -1) && (pwHash pw == targetHash) = Just pw
          | (width > -1) && (pwHash pw /= targetHash) = traverseChain table (width-1) targetHash (pwReduce $ pwHash $ pw)
          | otherwise = Nothing


-- Credit to G.G. Baker https://coursys.sfu.ca/2021su-cmpt-383-d1/pages/Assign1
generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

