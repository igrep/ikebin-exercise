{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString
import Data.Word
import qualified Data.ByteString as BS
import System.Environment
import Control.Applicative ((<$>), (*>), (<|>))

data Inst =
  MovAx Word8 | Interruption Word8 | SysWrite | SysExit | Arg Word8
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  bs <- BS.readFile $ fileName
  mapM_ putStrLn $ map showInst $ lexInsts ( BS.drop 16 bs )

lexInsts :: BS.ByteString -> [Inst]
lexInsts = leftError . parseOnly parseInsts

parseInsts :: Parser [Inst]
parseInsts = many1 $
      movAx
  <|> interruption
  <|> sysWrite
  <|> sysExit
  <|> arg

showInst :: Inst -> String
showInst (MovAx i)        = "mov ax, " ++ show i
showInst (Interruption i) = "int " ++ show i
showInst SysWrite         = "; sys write"
showInst SysExit          = "; sys exit"
showInst (Arg _)          = "; arg"

movAx :: Parser Inst
movAx = word8 0xb8 *> ( MovAx <$> beWord )

interruption :: Parser Inst
interruption = word8 0xcd *> ( Interruption <$> anyWord8 )

sysWrite :: Parser Inst
sysWrite = word8 0x04 *> return SysWrite

sysExit :: Parser Inst
sysExit = word8 0x04 *> return SysExit

arg :: Parser Inst
arg = Arg <$> beWord

beWord :: Parser Word8
beWord = do
  lo <- anyWord8
  ho <- anyWord8
  return $ ho * 256 + lo

leftError :: (Show a) => Either a b -> b
leftError = either ( error . show ) id
