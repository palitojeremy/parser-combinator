{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative (liftA2)
import           Data.Char
import           Data.Foldable       (for_)
import           Data.Functor
import qualified Data.HashMap.Strict as M
import           Data.List           (intercalate)
import           Prelude             hiding (any)
import           System.Environment
import           Text.Printf



-- data types

data ParseError = ParseError
  { errExpected :: String
  , errFound    :: String
  }

newtype Parser a = Parser { runParser :: String -> (String, Either ParseError a) }
  deriving (Functor)




-- instances

instance Show ParseError where
  show (ParseError e f) = printf "expected %s but found %s" e f

instance Applicative Parser where
  pure c = Parser (, Right c)
  pf <*> pa = Parser $ \s -> case runParser pf s of
    (s', Right f) -> fmap f <$> runParser pa s'
    (s', Left  r) -> (s', Left r)

instance Monad Parser where
  pa >>= f = Parser $ \s -> case runParser pa s of
    (s', Right a) -> runParser (f a) s'
    (s', Left  r) -> (s', Left r)



-- convenience run function

run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser (p <* eof) s



-- elementary parsers

any :: Parser Char
any = Parser $ \case
  []     -> ("", Left $ ParseError "any character" "the end of the input")
  (x:xs) -> (xs, Right x)

eof :: Parser ()
eof = Parser $ \s -> case s of
  []    -> ("", Right ())
  (c:_) -> (s, Left $ ParseError "the end of the input" [c])

parseError :: String -> String -> Parser a
parseError expected found = Parser (, Left $ ParseError expected found)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = try $ do
  c <- any
  if predicate c
    then pure c
    else parseError description [c]



-- backtracking

try :: Parser a -> Parser a
try p = Parser $ \s -> case runParser p s of
  (_s', Left err) -> (s, Left err)
  success         -> success

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s   -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success

choice :: String -> [Parser a] -> Parser a
choice description = foldr (<|>) noMatch
  where noMatch = parseError description "no match"



-- characters

char c = satisfy [c] (== c)
space  = satisfy "space" isSpace
digit  = satisfy "digit" isDigit

string :: String -> Parser String
string = traverse char



-- repetition

many, many1 :: Parser a -> Parser [a]
many  p = many1 p <|> pure []
many1 p = liftA2 (:) p $ many p

sepBy, sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy  p s = sepBy1 p s <|> pure []
sepBy1 p s = liftA2 (:) p $ many (s >> p)



-- syntax

spaces = many space
symbol s = string s <* spaces

between o c p = o *> p <* c
brackets = between (symbol "[") (symbol "]")
braces   = between (symbol "{") (symbol "}")



-- json

data JValue = JObject (M.HashMap String JValue)
            | JArray  [JValue]
            | JString String
            | JNumber Double
            | JBool   Bool
            | JNull

instance Show JValue where
  show = \case
    JNull     -> "null"
    JBool b   -> toLower <$> show b
    JNumber n -> show n
    JString s -> show s
    JArray  a -> show a
    JObject o -> printf "{%s}" $ intercalate ", " [printf "%s: %s" (show k) (show v) | (k,v) <- M.toList o]


json = spaces >> jValue
jValue = choice "a JSON value"
  [ JObject <$> jObject
  , JArray  <$> jArray
  , JString <$> jString
  , JNumber <$> jNumber
  , JBool   <$> jBool
  , JNull   <$  symbol "null"
  ]

jObject =
  fmap M.fromList $ braces $ jEntry `sepBy` symbol ","
  where jEntry = do
          k <- jString
          symbol ":"
          v <- jValue
          pure (k,v)

jArray = brackets $ jValue `sepBy` symbol ","

jString = between (char '"') (char '"') (many jChar) <* spaces
  where jChar = choice "JSON string character"
          [ try $ '\n' <$ string "\\n"  -- escaped newline
          , try $ '\t' <$ string "\\t"  -- escaped tab
          , try $ '"'  <$ string "\\\"" -- escaped quote
          , try $ '\\' <$ string "\\\\" -- escaped backslash
          , satisfy "not a quote" (/= '"')
          ]

jNumber = read <$> many1 digit

jBool = jTrue <|> jFalse
  where jTrue  = True  <$ symbol "true"
        jFalse = False <$ symbol "false"



-- example use case: attempts to parse the content of each given file

main :: IO ()
main = do
  args <- getArgs
  for_ args $ \filename -> do
    content <- readFile filename
    putStrLn content
    print $ run json content