-- | A small JSON value type with a correct encoder and decoder, so the
-- manifests, plans and site data CoScad emits are real JSON whatever the
-- part names, paths and hint values contain, and so tests and tools can
-- read them back. No dependency beyond base.
module Coscad.Json
  ( JValue (..)
  , encode
  , encodePretty
  , decode
  , jnum3
  , jobj
  , jlist
  , lookupKey
  , asString
  , asNumber
  , asArray
  ) where

import Data.Char (isDigit, isHexDigit, isSpace, ord, chr)
import Data.List (intercalate)
import Numeric (showHex, readHex)

data JValue
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Eq, Show)

-- | Numbers rounded to three decimals (a thousandth of a millimetre), with
-- whole numbers shown without a fraction. Non-finite values become null
-- rather than invalid JSON.
jnum3 :: Double -> JValue
jnum3 x
  | isNaN x || isInfinite x = JNull
  | otherwise = JNumber (fromIntegral (round (x * 1000) :: Integer) / 1000)

jobj :: [(String, JValue)] -> JValue
jobj = JObject

jlist :: [JValue] -> JValue
jlist = JArray

lookupKey :: String -> JValue -> Maybe JValue
lookupKey k (JObject kvs) = lookup k kvs
lookupKey _ _ = Nothing

asString :: JValue -> Maybe String
asString (JString s) = Just s
asString _ = Nothing

asNumber :: JValue -> Maybe Double
asNumber (JNumber n) = Just n
asNumber _ = Nothing

asArray :: JValue -> Maybe [JValue]
asArray (JArray xs) = Just xs
asArray _ = Nothing

-- | Compact encoding.
encode :: JValue -> String
encode v = case v of
  JNull -> "null"
  JBool b -> if b then "true" else "false"
  JNumber n -> number n
  JString s -> string s
  JArray xs -> "[" ++ intercalate ", " (map encode xs) ++ "]"
  JObject kvs -> "{" ++ intercalate ", " [string k ++ ": " ++ encode x | (k, x) <- kvs] ++ "}"

-- | Readable encoding: objects and arrays of objects one entry per line,
-- arrays of scalars inline.
encodePretty :: JValue -> String
encodePretty v = go 0 v ++ "\n"
  where
    go ind x = case x of
      JArray xs | not (null xs) && any composite xs ->
        "[\n" ++ intercalate ",\n" [pad (ind + 2) ++ go (ind + 2) y | y <- xs] ++ "\n" ++ pad ind ++ "]"
      JObject kvs | not (null kvs) ->
        "{\n" ++ intercalate ",\n" [pad (ind + 2) ++ string k ++ ": " ++ go (ind + 2) y | (k, y) <- kvs] ++ "\n" ++ pad ind ++ "}"
      _ -> encode x
    composite (JObject (_ : _)) = True
    composite (JArray (_ : _)) = True
    composite _ = False
    pad n = replicate n ' '

number :: Double -> String
number n
  | isNaN n || isInfinite n = "null"
  | n == fromIntegral r = show r
  | otherwise = show n
  where
    r = round n :: Integer

string :: String -> String
string s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c
      | ord c < 0x20 || c == '\x2028' || c == '\x2029' = "\\u" ++ pad4 (showHex (ord c) "")
      | otherwise = [c]
    pad4 h = replicate (4 - length h) '0' ++ h

-- | Parse a complete JSON document. Left names the byte offset and what
-- was expected.
decode :: String -> Either String JValue
decode input = case value (skip input) 0 of
  Left e -> Left e
  Right (v, rest, _) -> case skip rest of
    "" -> Right v
    r -> Left ("trailing input after the JSON value: " ++ take 20 r)
  where
    skip = dropWhile isSpace
    err :: Int -> String -> Either String a
    err off what = Left ("JSON: expected " ++ what ++ " at offset " ++ show off)
    value :: String -> Int -> Either String (JValue, String, Int)
    value s off = case s of
      'n' : 'u' : 'l' : 'l' : r -> Right (JNull, r, off + 4)
      't' : 'r' : 'u' : 'e' : r -> Right (JBool True, r, off + 4)
      'f' : 'a' : 'l' : 's' : 'e' : r -> Right (JBool False, r, off + 5)
      '"' : r -> do
        (str, r', n) <- stringBody r (off + 1) ""
        Right (JString str, r', n)
      '[' : r -> array (skip r) (off + 1) []
      '{' : r -> object (skip r) (off + 1) []
      c : _ | c == '-' || isDigit c -> numberTok s off
      _ -> err off "a JSON value"
    array s off acc = case s of
      ']' : r -> Right (JArray (reverse acc), r, off + 1)
      _ -> do
        (v, r, n) <- value s off
        case skip r of
          ',' : r' -> array (skip r') (n + 1) (v : acc)
          ']' : r' -> Right (JArray (reverse (v : acc)), r', n + 1)
          _ -> err n "',' or ']'"
    object s off acc = case s of
      '}' : r -> Right (JObject (reverse acc), r, off + 1)
      '"' : r -> do
        (k, r1, n1) <- stringBody r (off + 1) ""
        case skip r1 of
          ':' : r2 -> do
            (v, r3, n3) <- value (skip r2) (n1 + 1)
            case skip r3 of
              ',' : r4 -> object (skip r4) (n3 + 1) ((k, v) : acc)
              '}' : r4 -> Right (JObject (reverse ((k, v) : acc)), r4, n3 + 1)
              _ -> err n3 "',' or '}'"
          _ -> err n1 "':'"
      _ -> err off "a key or '}'"
    stringBody s off acc = case s of
      '"' : r -> Right (reverse acc, r, off + 1)
      '\\' : c : r -> case c of
        '"' -> stringBody r (off + 2) ('"' : acc)
        '\\' -> stringBody r (off + 2) ('\\' : acc)
        '/' -> stringBody r (off + 2) ('/' : acc)
        'n' -> stringBody r (off + 2) ('\n' : acc)
        'r' -> stringBody r (off + 2) ('\r' : acc)
        't' -> stringBody r (off + 2) ('\t' : acc)
        'b' -> stringBody r (off + 2) ('\b' : acc)
        'f' -> stringBody r (off + 2) ('\f' : acc)
        'u' | (h, r') <- splitAt 4 r, length h == 4, all isHexDigit h, [(n, "")] <- readHex h ->
          stringBody r' (off + 6) (chr n : acc)
        _ -> err off "a valid escape"
      c : r | ord c >= 0x20 -> stringBody r (off + 1) (c : acc)
      _ -> err off "a closing quote"
    numberTok s off =
      let (tok, r) = span (\c -> isDigit c || c `elem` "+-.eE") s
       in case reads (fixup tok) of
            [(n, "")] -> Right (JNumber n, r, off + length tok)
            _ -> err off "a number"
    -- Haskell's reads rejects leading '.', a bare leading '-' before '.', and '+' exponents are fine.
    fixup t = case t of
      '-' : '.' : rest -> "-0." ++ rest
      '.' : rest -> "0." ++ rest
      _ -> t
