{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString
  ( word8
  , anyWord8
  , parseOnly
  , Parser )

import qualified Data.Attoparsec.ByteString as ABS

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import System.Environment
import Control.Applicative ((<$>), (*>), (<|>))
import Control.Monad

data Program = Program Header [Inst]

data Header = Header
  { textSize :: Word16
  , dataSize :: Word16 }

data Inst =
  MovAx Word16 | Interruption Word8 | SysWrite | SysExit | Arg Word16
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  bs <- BS.readFile $ fileName
  let (Program _ is) = leftError $ parseOnly program bs
  mapM_ putStrLn $ map showInst $ is

program :: Parser Program
program = do
  h <- header
  is <- insts $ textSize h
  return $ Program h is

header :: Parser Header
header = do
  _ <- ABS.take 2
  t <- beWord16
  d <- beWord16
  _ <- ABS.take 10
  return $ Header t d

insts :: Word16 -> Parser [Inst]
insts tSize = replicateM (fromIntegral tSize) $
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
movAx = word8 0xb8 *> ( MovAx <$> beWord16 )

interruption :: Parser Inst
interruption = word8 0xcd *> ( Interruption <$> anyWord8 )

sysWrite :: Parser Inst
sysWrite = word8 0x04 *> return SysWrite

sysExit :: Parser Inst
sysExit = word8 0x01 *> return SysExit

arg :: Parser Inst
arg = Arg <$> beWord16

-- Big Endian Word16
beWord16 :: Parser Word16
beWord16 = do
  lo <- anyWord8
  ho <- anyWord8
  return $ shift (fromIntegral ho) 8 .|. (fromIntegral lo)

leftError :: (Show a) => Either a b -> b
leftError = either ( error . show ) id

showBits :: Bits a => a -> String
showBits bits = reverse $ take (bitSize bits) $ map f [0..]
  where
    f n = if testBit bits n then '1' else '0'
