{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, PatternGuards #-}
--------------------------------------------------------------------
-- |
-- Module    : Codec.MIME.Pare
-- Copyright : (c) 2006-2009, Galois, Inc. 
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@galois.com>
-- Stability : provisional
-- Portability: portable
--
-- Parsing MIME content.
-- 
--------------------------------------------------------------------
module Codec.MIME.Parse
  ( ParseMIME
  , parseMIMEBody
  , parseMIMEType
  , safeParseMIMEBodyByteString
  , WithoutCRLF(..)
  ) where

import Codec.MIME.Type
import Codec.MIME.Decode
import Control.Arrow ( first, second, (***) )
import Data.ByteString.Search.BoyerMoore ( matchSL )
import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace ( trace )
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy ( ByteString )

import System.IO.Unsafe (unsafePerformIO) -- REMOVE

class ParseMIME a where
  splitMulti :: String -> a -> [a]
  decodeBody :: String -> a -> a
  parseHeaders :: a -> ([(String,String)], a)

instance ParseMIME String where
  splitMulti = splitMultiS
  decodeBody = decodeBodyS
  parseHeaders = parseHeadersS

instance ParseMIME ByteString where
  splitMulti = splitMultiB
  decodeBody = decodeBodyB
  parseHeaders = first (map (C.unpack *** C.unpack)) . parseHeadersB

newtype WithoutCRLF = WithoutCRLF { withoutCRLF :: C.ByteString }

instance ParseMIME WithoutCRLF where
  splitMulti bnd = map WithoutCRLF . splitMultiB' bnd . withoutCRLF
  decodeBody x = WithoutCRLF . decodeBodyB x . withoutCRLF
  parseHeaders = second WithoutCRLF . first (map (C.unpack *** C.unpack)) . parseHeadersB' . withoutCRLF

parseMIMEBody :: ParseMIME a => [(String,String)] -> a -> MIMEValue a
parseMIMEBody headers_in body =
  case mimeType mty of
    Multipart{} -> parseMultipart mty body
    Message{}   -> parseMultipart mty body
    _           -> nullMIMEValue 
                    { mime_val_type    = mty
	            , mime_val_disp    = parseContentDisp headers
	            , mime_val_content = Single (processBody headers body)
 	           }

 where headers = [ (map toLower k,v) | (k,v) <- headers_in ]
       mty = fromMaybe defaultType
                       (parseContentType =<< lookupField "content-type" headers)

safeParseMIMEBodyByteString :: [(String,String)] -> ByteString -> MIMEValue ByteString
safeParseMIMEBodyByteString headers body = if mb == C.pack `fmap` ms then mb else unsafePerformIO err
  where ms = parseMIMEBody headers (C.unpack body)
        mb = parseMIMEBody headers body

        err = do C.writeFile "/tmp/mime-exception.log" $
                    C.append (C.pack $ show headers) $
                    C.append (C.pack "\nBODY:\n") $ C.append body $
                    C.append (C.pack "\nSTRING BASED CONTENTS:\n") $
                    C.append (C.pack $ concat $ contents $ mime_val_content ms) $
                    C.append (C.pack "\nBYTESTRING BASED CONTENTS:\n") $
                    C.append (C.concat $ contents $ mime_val_content mb) $ C.pack "\n"
                 error "safeParseMIMEBodyByteString: bug"

        contents (Single x) = [x]
        contents (Multi xs) = concatMap (contents . mime_val_content) xs

defaultType :: Type
defaultType = Type { mimeType   = Text "plain"
                   , mimeParams = [("charset", "us-ascii")]
                   }

parseContentDisp :: [(String,String)] -> Maybe Disposition
parseContentDisp headers =
  (processDisp . dropFoldingWSP) =<< lookupField "content-disposition" headers
  where
  processDisp "" = Nothing
  processDisp xs = Just $
    case break (\ch -> isSpace ch || ch == ';') xs of
      (as,"") -> Disposition { dispType = toDispType (map toLower as)
                             , dispParams = []
                             }
      (as,bs) -> Disposition { dispType = toDispType (map toLower as)
                             , dispParams = processParams (parseParams bs)
                             }

  processParams = map procP
    where
    procP (as,val)
      | "name" == asl              = Name val
      | "filename" == asl          = Filename val
      | "creation-date" == asl     = CreationDate val
      | "modification-date" == asl = ModDate val
      | "read-date" == asl         = ReadDate val
      | "size" == asl              = Size val
      | otherwise                  = OtherParam (map toLower as) val
      where asl = map toLower as

  toDispType t = case t of
                   "inline"     -> DispInline
                   "attachment" -> DispAttachment
                   "form-data"  -> DispFormData
                   _            -> DispOther t


processBody :: ParseMIME a => [(String,String)] -> a -> a
processBody = maybe id decodeBody . lookupField "content-transfer-encoding"

parseMIMEMessage :: ParseMIME a => a -> MIMEValue a
parseMIMEMessage = uncurry parseMIMEBody . parseHeaders

parseHeadersS :: String -> ([(String,String)], String)
parseHeadersS str =
  case findFieldName "" str of
    Left (nm, rs) -> parseFieldValue nm (dropFoldingWSP rs)
    Right body    -> ([],body)
 where
  findFieldName _acc "" = Right ""
  findFieldName _acc ('\r':'\n':xs) = Right xs
  findFieldName acc (':':xs) = Left (reverse (dropWhile isHSpace acc), xs)
  findFieldName acc (x:xs) = findFieldName (x:acc) xs

  parseFieldValue nm xs =
    case takeUntilCRLF xs of
      (as,"") -> ([(nm,as)],"")
      (as,bs) -> let (zs,ys) = parseHeadersS bs in ((nm,as):zs,ys)

parseHeadersB :: ByteString -> ([(ByteString,ByteString)], ByteString)
parseHeadersB str =
  case findFieldName str of
    Left (nm, rs) -> parseFieldValue nm (dropFoldingWSPB rs)
    Right body    -> ([],body)
 where
  findFieldName s =
    --if C.null s then Right C.empty else
    let (xs, ys) = C.break (\c-> c=='\r' || c==':') s in
    case C.uncons ys of
      Just (':', ys') -> Left (C.dropWhile isHSpace xs, ys')
      Just ('\r', ys') | Just ('\n', ys'') <- C.uncons ys' -> Right ys''
      _ -> Right C.empty -- here one forget the whole 's' as in the string code

  parseFieldValue nm xs =
    case takeUntilCRLFB xs of
      (as,bs) | C.null bs -> ([(nm,as)],bs)
              | otherwise -> let (zs,ys) = parseHeadersB bs in ((nm,as):zs,ys)

parseHeadersB' :: ByteString -> ([(ByteString,ByteString)], ByteString)
parseHeadersB' str =
  case findFieldName str of
    Left (nm, rs) -> parseFieldValue nm (dropFoldingWSPB' rs)
    Right body    -> ([],body)
 where
  findFieldName s =
    --if C.null s then Right C.empty else
    let (xs, ys) = C.break (\c-> c=='\n' || c==':') s in
    case C.uncons ys of
      Just (':', ys') -> Left (C.dropWhile isHSpace xs, ys')
      Just ('\n', ys') -> Right ys'
      _ -> Right C.empty -- here one forget the whole 's' as in the string code

  parseFieldValue nm xs =
    case takeUntilCRLFB' xs of
      (as,bs) | C.null bs -> ([(nm,as)],bs)
              | otherwise -> let (zs,ys) = parseHeadersB' bs in ((nm,as):zs,ys)

parseMultipart :: ParseMIME a => Type -> a -> MIMEValue a
parseMultipart mty body =
  case lookup "boundary" (mimeParams mty) of
    Nothing -> trace ("Multipart mime type, " ++ showType mty ++
      ", has no required boundary parameter. Defaulting to text/plain") $
      nullMIMEValue{ mime_val_type = defaultType
                   , mime_val_disp = Nothing
		   , mime_val_content = Single body
		   }
    Just bnd -> nullMIMEValue { mime_val_type = mty
                              , mime_val_disp = Nothing
			      , mime_val_content = Multi $ map parseMIMEMessage $ splitMulti bnd body
			      }

splitMultiS :: String -> String -> [String]
splitMultiS bnd body_in =
  -- Note: we insert a CRLF if it looks as if the boundary string starts
  -- right off the bat.  No harm done if this turns out to be incorrect.
  let body = case body_in of
                '-':'-':_ -> ('\r':'\n':body_in)
                _ -> body_in
  in case untilMatch dashBoundary body of
       Nothing           -> []
       Just ('-':'-':_)  -> [] -- dropTrailer ?
       Just xs           -> splitMulti1 $ dropTrailer xs

 where
  dashBoundary = "\r\n--" ++ bnd

  splitMulti1 xs =
    case matchUntil dashBoundary xs of
      ("","") -> []
      (ys,"") -> [ys]
      (ys,'-':'-':_) -> [ys]
      (ys,zs) -> ys : splitMulti1 (dropTrailer zs)

  dropTrailer xs =
    case dropWhile isHSpace xs of
      '\r':'\n':xs1 -> xs1
      xs1 -> xs1 -- hmm, flag an error?

splitMultiB :: String -> ByteString -> [ByteString]
splitMultiB bnd body_in =
  -- Note: we insert a CRLF if it looks as if the boundary string starts
  -- right off the bat.  No harm done if this turns out to be incorrect.
  let body = if boundaryStart body_in then crlfB `C.append` body_in else body_in in
  let (xs, ys) = matchDashBnd body in
  if (C.null xs && C.null ys) || boundaryStart ys then []
  else splitMulti1 $ dropTrailer ys

 where
  dashBoundary = S.pack $ "\r\n--" ++ bnd
  dashBoundaryLen = fromIntegral $ S.length dashBoundary

  matchDashBnd s = maybe (s, C.empty)
                         (second (C.drop dashBoundaryLen) . (`C.splitAt` s))
                   $ listToMaybe $ matchSL dashBoundary s

  splitMulti1 xs =
    let (ys, zs) = matchDashBnd xs in
    if C.null zs then if C.null ys then [] else [ys]
                 else if boundaryStart zs then [ys] else ys : splitMulti1 (dropTrailer zs)

  boundaryStart x = C.length (C.takeWhile (=='-') x) >= 2

  dropTrailer xs =
    let xs1 = C.dropWhile isHSpace xs in
    if C.take 2 xs1 == crlfB then C.drop 2 xs1 else xs1 -- hmm, flag an error?

splitMultiB' :: String -> ByteString -> [ByteString]
splitMultiB' bnd body_in =
  -- Note: we insert a '\n' if it looks as if the boundary string starts
  -- right off the bat.  No harm done if this turns out to be incorrect.
  let body = if boundaryStart body_in then '\n' `C.cons` body_in else body_in in
  let (xs, ys) = matchDashBnd body in
  if (C.null xs && C.null ys) || boundaryStart ys then []
  else splitMulti1 $ dropTrailer ys

 where
  dashBoundary = S.pack $ "\n--" ++ bnd
  dashBoundaryLen = fromIntegral $ S.length dashBoundary

  matchDashBnd s = maybe (s, C.empty)
                         (second (C.drop dashBoundaryLen) . (`C.splitAt` s))
                   $ listToMaybe $ matchSL dashBoundary s

  splitMulti1 xs =
    let (ys, zs) = matchDashBnd xs in
    if C.null zs then if C.null ys then [] else [ys]
                 else if boundaryStart zs then [ys] else ys : splitMulti1 (dropTrailer zs)

  boundaryStart x = C.length (C.takeWhile (=='-') x) >= 2

  dropTrailer xs =
    let xs' = C.dropWhile isHSpace xs in
    case C.uncons xs' of
      Just ('\n', xs'') -> xs''
      _ -> xs' -- hmm, flag an error?

crlfB :: ByteString
crlfB = C.pack "\r\n"

parseMIMEType :: String -> Maybe Type
parseMIMEType = parseContentType

parseContentType :: String -> Maybe Type
parseContentType str =
   case break (=='/') (dropFoldingWSP str) of
       (maj,_:minor) ->
          case break (\ ch -> isHSpace ch || isTSpecial ch) minor of
            (as,bs)  -> 
               Just Type { mimeType = toType maj as
                         , mimeParams = parseParams (dropWhile isHSpace bs)
                         }
       _ -> trace ("unable to parse content-type: " ++ show str) $ Nothing
 where
  toType a b = case lookupField (map toLower a) mediaTypes of
                 Just ctor -> ctor b
                 _ -> Other a b


parseParams :: String -> [(String,String)]
parseParams "" = []
parseParams (';':xs) =
    case break (=='=') (dropFoldingWSP xs) of
      (_,[]) -> []
      (nm_raw,_:vs) ->
        case vs of
          '"':vs1 ->
             case break (=='"') vs1 of
               (val,"") -> [(nm,val)]
               (val,_:zs) -> (nm,val):parseParams (dropWhile isHSpace zs)
          _ -> case break (\ ch -> isHSpace ch || isTSpecial ch) vs of
                 (val,zs) -> (nm,val):parseParams (dropWhile isHSpace zs)
       where
        nm = map toLower nm_raw

parseParams cs = trace ("Codec.MIME.Parse.parseParams: curious param value -- " ++ show cs) []

mediaTypes :: [(String, String -> MIMEType)]
mediaTypes =
  [ ("multipart",   (Multipart . toMultipart))
  , ("application", Application)
  , ("audio",       Audio)
  , ("image",       Image)
  , ("message",     Message)
  , ("model",       Model)
  , ("text",        Text)
  , ("video",       Video)
  ]
 where toMultipart b = fromMaybe other (lookupField (map toLower b) multipartTypes)
          where other = case b of
                          'x':'-':_ -> Extension b
                          _ -> OtherMulti b


multipartTypes :: [(String, Multipart)]
multipartTypes =
  [ ("alternative", Alternative)
  , ("byteranges",  Byteranges)
  , ("digest",      Digest)
  , ("encrypted",   Encrypted)
  , ("form-data",   FormData)
  , ("mixed",       Mixed)
  , ("parallel",    Parallel)
  , ("related",     Related)
  , ("signed",      Signed)
  ]

untilMatch :: String -> String -> Maybe String
untilMatch _   "" = Nothing
untilMatch str xs
    -- slow, but it'll do for now.
  | str `isPrefixOf` xs = Just $ drop (length str) xs
untilMatch str (_:xs) = untilMatch str xs

matchUntil :: String -> String -> (String, String)
matchUntil _   "" = ("", "")
matchUntil str xs
    -- slow, but it'll do for now.
  | str `isPrefixOf` xs = ("", drop (length str) xs)
matchUntil str (x:xs) = let (as,bs) = matchUntil str xs in (x:as,bs)



isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

isTSpecial :: Char -> Bool
isTSpecial x = x `elem` "()<>@,;:\\\"/[]?="


dropFoldingWSP :: String -> String
dropFoldingWSP "" = ""
dropFoldingWSP (x:xs)
 | isHSpace x = dropFoldingWSP xs
dropFoldingWSP ('\r':'\n':x:xs)
 | isHSpace x = dropFoldingWSP xs
dropFoldingWSP xs = xs

dropFoldingWSPB :: ByteString -> ByteString
dropFoldingWSPB s =
  case C.uncons s of
    Just (x, xs) | isHSpace x -> dropFoldingWSPB xs
    Just ('\r', s1) | Just ('\n', s2) <- C.uncons s1
                    , Just (c, s3) <- C.uncons s2
                    , isHSpace c -> dropFoldingWSPB s3
    _ -> s

dropFoldingWSPB' :: ByteString -> ByteString
dropFoldingWSPB' s =
  case C.uncons s of
    Just (x, xs) | isHSpace x -> dropFoldingWSPB' xs
    Just ('\n', s1) | Just (c, s2) <- C.uncons s1
                    , isHSpace c -> dropFoldingWSPB' s2
    _ -> s

-- DEBUG
{-
prop_same_sem :: (Functor f, Eq (f ByteString)) => (ByteString -> f ByteString) -> (String -> f String) -> ByteString -> Bool
prop_same_sem f g s = f s == fmap C.pack (g (C.unpack s))

prop_same_sem' :: (ByteString -> ByteString) -> (String -> String) -> String -> Bool
prop_same_sem' f g s = f (C.pack s) == C.pack (g s)
-}

-- takeUntilCRLF "\r\n" I think that this should return ("", "")
takeUntilCRLF :: String -> (String, String)
takeUntilCRLF str = go "" str
 where
  go acc "" = (reverse (dropWhile isHSpace acc), "")
  go acc ('\r':'\n':x:xs)
   | isHSpace x = go (' ':acc) xs
   | otherwise  = (reverse (dropWhile isHSpace acc), x:xs)
  go acc (x:xs) = go (x:acc) xs

{-
splitCRLFB :: ByteString -> (Bool, ByteString, ByteString)
splitCRLFB str =
    let (str1, str2) = C.break (=='\r') str in
    if C.take 2 str2 == crlfB
    then (True, str1, C.drop 2 str2)
    else (False, str1, str2)
-}

takeUntilCRLFB :: ByteString -> (ByteString, ByteString)
takeUntilCRLFB = go []
 where
  go acc str =
    let (str1, str2) = C.break (=='\r') str in
    case C.uncons str2 of
      Just ('\r', str3) | Just ('\n', str4) <- C.uncons str3 ->
         case C.uncons str4 of
           Just (x, xs) | isHSpace x -> go (str1:acc) xs
                        | otherwise  -> (finish (str1:acc), str4)
           Nothing -> (crlfB, C.empty) -- I would prefer (C.empty, C.empty)
      Just _ -> go (str1:acc) str2
      Nothing -> (finish (str1:acc), C.empty)
{-
    let (str1, str2) = C.break (=='\r') str in
    if C.take 2 str2 == crlfB then
      let str3 = C.drop 2 str2 in
      case C.uncons str3 of
        Just (x, xs) | isHSpace x -> go (str1:acc) xs
                     | otherwise  -> (finish (str1:acc), str3)
        Nothing -> (crlfB, C.empty) -- I would prefer (C.empty, C.empty)
    else if C.null 
-}
{-
    case splitCRLFB str of
      Just (str1, str3) ->
        case C.uncons str3 of
          Just (x, xs) | isHSpace x -> go (str1:acc) xs
                       | otherwise  -> (finish (str1:acc), str3)
          Nothing -> (crlfB, C.empty) -- I would prefer (C.empty, C.empty)
      Nothing -> (finish (str1:acc), C.empty)

-}
  finish = C.unwords . reverse

takeUntilCRLFB' :: ByteString -> (ByteString, ByteString)
takeUntilCRLFB' = go []
 where
  go acc str =
    let (str1, str2) = C.break (=='\n') str in
    case C.uncons str2 of
      Just ('\n', str3) ->
         case C.uncons str3 of
           Just (x, xs) | isHSpace x -> go (str1:acc) xs
                        | otherwise  -> (finish (str1:acc), str3)
           Nothing -> (C.pack "\n", C.empty) -- I would prefer (C.empty, C.empty)
      Just _ -> go (str1:acc) str2
      Nothing -> (finish (str1:acc), C.empty)
  finish = C.unwords . reverse

-- case in-sensitive lookup of field names or attributes\/parameters.
lookupField :: String -> [(String,a)] -> Maybe a
lookupField n ns = 
   -- assume that inputs have been mostly normalized already 
   -- (i.e., lower-cased), but should the lookup fail fall back
   -- to a second try where we do normalize before giving up.
  case lookup n ns of
    x@Just{} -> x
    Nothing  -> 
      let nl = map toLower n in
      case find (\ (y,_) -> nl == map toLower y) ns of
        Nothing -> Nothing
	Just (_,x)  -> Just x
      
