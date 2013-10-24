import Text.Parsec (ParseError)
import Text.Parsec.ByteString (Parser)
import Data.Word
import qualified Data.ByteString as BS
import System.Environment

data Inst =
  MovAx Word16 | SysCall Word8 | Arg Word16
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs
  bs <- BS.readFile $ head args
  mapM_ putStrLn $ map showInsts $ lexInsts bs

lexInsts :: BS.ByteString -> [Inst]
lexInsts = leftError . eitherLexInsts

eitherLexInsts :: BS.ByteString -> Parser ParseError [Inst]

leftError :: (Show a) => Either a b -> b
leftError = either ( error . show ) id
