--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Decode
-- Copyright : (c) 2006-2009, Galois, Inc. 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- 
-- 
--------------------------------------------------------------------

module Codec.MIME.Decode where

import Data.Char

import Codec.MIME.QuotedPrintable as QP
import Codec.MIME.Base64 as Base64
import Data.ByteString.Lazy (ByteString)
-- import qualified Data.ByteString.Lazy

-- | @decodeBody enc str@ decodes @str@ according to the scheme
-- specified by @enc@. Currently, @base64@ and @quoted-printable@ are
-- the only two encodings supported. If you supply anything else
-- for @enc@, @decodeBody@ returns @str@.
-- 
decodeBodyS :: String -> String -> String
decodeBodyS enc =
 case map toLower enc of
   "base64"           -> map (chr.fromIntegral) . Base64.decode
   "quoted-printable" -> QP.decode
   _ -> id

decodeBodyB :: String -> ByteString -> ByteString
decodeBodyB enc =
 case map toLower enc of
   "base64"           -> Base64.decodeB
   "quoted-printable" -> QP.decodeB
   _ -> id

-- Decoding of RFC 2047's "encoded-words" production
-- (as used in email-headers and some HTTP header cases
-- (AtomPub's Slug: header))
decodeWord :: String -> Maybe (String, String)
decodeWord str =
  case str of
   '=':'?':xs ->
     case dropLang $ break (\ch -> ch =='?' || ch == '*') xs of
       (cs,_:x:'?':bs)
         | isKnownCharset (map toLower cs) ->
           case toLower x of
             'q' -> decodeQletter cs (break (=='?') bs)
             'b' -> decodeBletter cs (break (=='?') bs)
             _   -> Nothing
       _ -> Nothing
   _ -> Nothing
 where
  isKnownCharset cs = cs `elem` ["iso-8859-1", "us-ascii"]

   -- ignore RFC 2231 extension of permitting a language tag to be supplied
   -- after the charset.
  dropLang (as,'*':bs) = (as,dropWhile (/='?') bs)
  dropLang (as,bs) = (as,bs)

  decodeQletter cset (fs,'?':'=':rs) = Just (fromCharset cset (QP.decode fs),rs)
  decodeQletter _ _ = Nothing

  decodeBletter cset (fs,'?':'=':rs) =
    Just (fromCharset cset (Base64.decodeToString fs),rs)
  decodeBletter _ _ = Nothing

  fromCharset _cset cs = cs

decodeWords :: String -> String
decodeWords "" = ""
decodeWords (x:xs)
 | isSpace x = x : decodeWords xs
 | otherwise =
  case decodeWord (x:xs) of
    Nothing -> x : decodeWords xs
    Just (as,bs) -> as ++ decodeWords bs


