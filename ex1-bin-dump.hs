import qualified Data.ByteString as BS
import System.Environment
import Data.List.Split
import Numeric
import Data.List

main :: IO ()
main = do
  args <- getArgs
  bs <- BS.readFile $ head args
  let whenLoadedOnRam = length args >= 2 && last args == "-m"
  let chunkSize = 16
  let bytesInHex = map ( myShowHex 2 ) $ drop16If whenLoadedOnRam $ BS.unpack bs

  mapM_ putStrLn $ prependAddresses chunkSize $ map ( intercalate " " ) $ chunksOf chunkSize bytesInHex

myShowHex :: (Integral a, Show a) => Int -> a -> String
myShowHex size i =
  if l < size
    then replicate paddingSize '0' ++ s
    else s
  where
    s = showHex i ""
    l = length s
    paddingSize = size - l

drop16If :: Bool -> [a] -> [a]
drop16If True = drop 16
drop16If _    = id

prependAddresses :: Int -> [String] -> [String]
prependAddresses size = zipWith prepend hexes
  where
    hexes = map ( myShowHex 8 ) [0, size..]
    prepend s1 s2 = s1 ++ " " ++ s2
