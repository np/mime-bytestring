--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.QuotedPrintable
-- Copyright : (c) 2006-2009, Galois, Inc. 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability:
--
-- 
--
--------------------------------------------------------------------
module Codec.MIME.QuotedPrintable 
       ( decode  -- :: String -> String
       , decodeB -- :: ByteString -> ByteString
       , encode  -- :: String -> String
       ) where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad (guard)
import Control.Arrow (second)
import Data.Maybe (fromMaybe)
--import Test.QuickCheck

-- | 'decode' incoming quoted-printable content, stripping
-- out soft line breaks and translating @=XY@ sequences
-- into their decoded byte\/octet. The output encoding\/representation 
-- is still a String, not a sequence of bytes.
decode :: String -> String
decode "" = ""
decode ('=':'\r':'\n':xs) = decode xs -- soft line break.
decode ('=':x1:x2:xs)
 | isHexDigit x1 && isHexDigit x2 =
    chr (digitToInt x1 * 16 + digitToInt x2) : decode xs
decode ('=':xs) = '=':decode xs
              -- make it explicit that we propagate other '=' occurrences.
decode (x1:xs) = x1:decode xs

-- | 'encode' converts a sequence of characeter _octets_ into
-- quoted-printable form; suitable for transmission in MIME
-- payloads. Note the stress on _octets_; it is assumed that
-- you have already converted Unicode into a <=8-bit encoding
-- (UTF-8, most likely.)
encode :: String -> String
encode xs = encodeLength 0 xs

-- | @encodeLength llen str@ is the worker function during encoding.
-- The extra argument @llen@ tracks the current column for the line
-- being processed. Soft line breaks are inserted if a line exceeds
-- a max length.
encodeLength :: Int -> String -> String
encodeLength _ "" = ""
encodeLength n (x:xs)
 | n >= 72  = '=':'\r':'\n':encodeLength 0 (x:xs)
encodeLength _ ('=':xs) 
 = '=':'3':'D':encodeLength 0 xs
encodeLength n (x:xs)
 | ox >= 0x100 = error ("QuotedPrintable.encode: encountered > 8 bit character: " ++ show (x,ox))
 | n >= 72     = '=':'\r':'\n':encodeLength 0 (x:xs)
 | ox >= 0x21 && ox <= 0x7e = x : encodeLength (n+1) xs
 | ox == 0x09 || ox == 0x20 = x : encodeLength (n+1) xs
 | otherwise = '=':showH (ox `div` 0x10): showH (ox `mod` 0x10):encodeLength (n+3) xs
 where
  ox = ord x
  showH v
   | v < 10    = chr (ord_0 + v)
   | otherwise = chr (ord_A + (v-10))
   
  ord_0 = ord '0'
  ord_A = ord 'A'

decodeB :: C.ByteString -> C.ByteString
decodeB s =
  let (before, after) = second (C.drop 1) $ C.break (=='=') s in
  if C.null after then s -- we propagate other '=' occurrences.
  else C.append before $
   fromMaybe (C.cons '=' $ decodeB after) $ -- make it explicit that we propagate other '=' occurrences.
     do (x1, after1) <- C.uncons after
        (x2, after2) <- C.uncons after1
        if x1 == '\r' && x2 == '\n'
         then return $ decodeB after2
         else do guard (isHexDigit x1 && isHexDigit x2)
                 return $ chr (digitToInt x1 * 16 + digitToInt x2) `C.cons` decodeB after2

{-
preferOneOf :: [a] -> Either Int a -> a
preferOneOf choices = either ((choices !!) . (`mod` length choices)) id

prop_decode s = decode s == (C.unpack . decodeB . C.pack $ s)
prop_decode' = prop_decode . map (preferOneOf ('=':'\r':'\n':' ':['0'..'9']++['a'..'z']))
-}