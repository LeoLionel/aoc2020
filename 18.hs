import Text.Parsec
import Text.Parsec.String
import Data.Either


main :: IO ()
main = do
  input <- lines <$> readFile "input18"
  print $ solve  input 
  print $ solve2 input


data Expr = Num Int | BinOp Op Expr Expr deriving Show
data Op = Plus | Mult deriving Show


num :: Parser Expr
num = do
  n <- try $ spaces >> many1 digit
  return $ Num (read n :: Int)


plus :: Parser Op
plus = try $ spaces >> char '+' >> return Plus


mult :: Parser Op
mult = try $ spaces >> char '*' >> return Mult


braces :: Parser a -> Parser a
braces = between (spaces >> char '(') (spaces >> char ')') 


expr :: Parser Expr
expr = do 
  x <- num <|> braces expr
  rest x


rest :: Expr -> Parser Expr
rest x = do  
  op <- plus <|> mult
  y  <- num  <|> braces expr
  rest (BinOp op x y)
  <|> return x


eval :: Expr -> Int
eval expr =
  case expr of
    (Num x)          -> x
    (BinOp Plus x y) -> (eval x) + (eval y)
    (BinOp Mult x y) -> (eval x) * (eval y)


solve :: [String] -> Int
solve = sum . map eval . rights . map (parse expr "")


-- part2


expr2 :: Parser Expr
expr2 = factor >>= restProduct


factor :: Parser Expr
factor = summand >>= restSum

summand :: Parser Expr
summand = num <|> braces expr2


restSum :: Expr -> Parser Expr 
restSum x = do
  plus
  y <- summand
  restSum (BinOp Plus x y)
  <|> return x


restProduct :: Expr -> Parser Expr 
restProduct x = do
  mult
  y <- factor
  restProduct (BinOp Mult x y)
  <|> return x


solve2 :: [String] -> Int
solve2 = sum . map eval . rights . map (parse expr2 "")
