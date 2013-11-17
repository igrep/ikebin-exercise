{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import qualified Text.Parsec.ByteString as PBS
-- TODO: CharからWord16に変換する方法がわからないのでひとまずIntで代用。
-- import Data.Word
import qualified Data.ByteString.Char8 as BS
import System.Environment
import Control.Applicative ((<$>), (*>))
import Data.Char (ord)

data Inst =
  MovAx Int | Interruption Int | SysWrite | SysExit | Arg Int
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  bs <- BS.readFile $ fileName
  mapM_ putStrLn $ map showInst $ lexInsts fileName ( BS.drop 16 bs )

lexInsts :: SourceName -> BS.ByteString -> [Inst]
lexInsts sn = leftError . parse parseInsts sn

parseInsts :: PBS.Parser [Inst]
parseInsts = many1 $
      movAx
  <|> interruption
  <|> sysWrite
  <|> sysExit
  <|> arg

showInst :: Inst -> String
showInst (MovAx i)        = "mov ax, " ++ show i ++ "\n"
showInst (Interruption i) = "int " ++ show i ++ "\n"
showInst SysWrite         = "; sys write\n"
showInst SysExit          = "; sys exit\n"
showInst (Arg _)          = "; arg"

movAx :: PBS.Parser Inst
movAx = char '\xb8' *> ( MovAx <$> beWord )

interruption :: PBS.Parser Inst
interruption = char '\xcd' *> ( Interruption <$> byte )

sysWrite :: PBS.Parser Inst
sysWrite = char '\x04' *> return SysWrite

sysExit :: PBS.Parser Inst
sysExit = char '\x04' *> return SysExit

arg :: PBS.Parser Inst
arg = Arg <$> beWord

beWord :: PBS.Parser Int
beWord = do
  ho <- byte
  lo <- byte
  return $ ho * 256 + lo

byte :: PBS.Parser Int
byte = ord <$> anyChar

leftError :: (Show a) => Either a b -> b
leftError = either ( error . show ) id
