{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString
  ( word8
  , anyWord8
  , parseOnly
  , many1
  , parse
  , Parser
  , IResult(..) )

import qualified Data.Attoparsec.ByteString as ABS

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import System.Environment
import Control.Applicative ((<$>), (*>), (<|>))

data Header = Header
  { textSize :: Word16
  , _dataSize :: Word16 }

data Inst =
  MovAx Word16 | Interruption Word8 | SysWrite | SysExit | Arg Word16
  deriving (Show, Read)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  bs <- BS.readFile $ fileName
  let (h, leftBs) = leftError $ splitHeader bs
  let ts = fromIntegral $ textSize h
  mapM_ putStrLn $ map showInst $ leftError $ parseOnly insts $ BS.take ts leftBs

header :: Parser Header
header = do
  _ <- ABS.take 2
  t <- beWord16
  d <- beWord16
  _ <- ABS.take 10
  return $ Header t d

splitHeader :: BS.ByteString -> Either String (Header, BS.ByteString)
splitHeader bs = handle $ parse header bs
  where
    handle (Done leftBs h) = Right (h, leftBs)
    handle _ = Left "Bad Header!"

insts :: Parser [Inst]
insts = many1 $
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

_showBits :: Bits a => a -> String
_showBits bits = reverse $ take (bitSize bits) $ map f [0..]
  where
    f n = if testBit bits n then '1' else '0'
