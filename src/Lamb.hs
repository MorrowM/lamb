{-# LANGUAGE OverloadedStrings #-}
module Lamb
  ( reduce
  , zero
  , suc
  , fromInt
  , toInt
  , plus
  , ($$)
  , ($.)
  , mult
  , pow
  , sub
  , parseTerm
  ) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Char
import           Data.String
import           Data.Text           (Text, pack, unpack)
import           Text.Parsec         hiding ((<|>))
import           Text.Parsec.String

data Term = Var Text |  Lam Text Term | App Term Term
  deriving Eq

instance IsString Term where
  fromString s = either (error . show) id $ parse (term <* eof) "" s

parseTerm :: String -> Either String Term
parseTerm = first show . parse (term <* eof) "<interactive>"

instance Show Term where
  show = pp
    where
      par s = "(" <> s <> ")"
      ppApp l@Lam {} = par $ pp l
      ppApp a@App {} = pp a
      ppApp v@Var {} = pp v
      pp (Var x)          = unpack x
      pp (Lam x e)        = "λ" <> unpack x <> ". " <> pp e
      pp (App e a@App {}) = pp e <> " " <> par (pp a)
      pp (App e e')       =  ppApp e <> " " <> ppApp e'


ident :: Parser Text
ident = pack <$> many1 (satisfy (\c -> c == '\'' || isAlphaNum c))

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

group :: Parser Term
group = parens term

var :: Parser Term
var = Var <$> ident

lam :: Parser Term
lam = Lam <$> (lambda *> ident) <*> (char '.' *> term)
  where lambda = char '\\' <|> char 'λ'
app :: Parser Term
app = foldl1 App <$> sepBy1 (var <|> group) (char ' ')

term :: Parser Term
term = spaces *> (lam <|> try app <|> var) <* spaces

replaceVar :: Text -> Term -> Term -> Term
replaceVar x e (Var v) | v == x = e
replaceVar x e (Lam y e') =
  let (name, subterm)
        | x == y = (y <> "\'", replaceVar y (Var (y <> "\'")) e')
        | otherwise = (y, e')
  in Lam name (replaceVar x e subterm)
replaceVar x e (App a b) = App (replaceVar x e a) (replaceVar x e b)
replaceVar _ _ t = t

betaReduce :: Term -> Maybe Term
betaReduce (Lam x e)          = Lam x <$> betaReduce e
betaReduce (App (Lam x e) e') = Just $ replaceVar x e' e
betaReduce (App e e')         = App <$> betaReduce e <*> Just e'
                                <|> App <$> Just e <*> betaReduce e'
betaReduce _                  = Nothing

etaReduce :: Term -> Maybe Term
etaReduce (Lam x (App e (Var x'))) | x == x' = Just e
etaReduce _ = Nothing

reduce :: Term -> Term
reduce e = maybe e reduce $ betaReduce e <|> etaReduce e

zero :: Term
zero = "\\f. \\x. x"

suc :: Term
suc = "\\n. \\f. \\x. f (n f x)"

fromInt :: Integer -> Term
fromInt 0 = zero
fromInt n = reduce $ App suc (fromInt $ n - 1)

plus :: Term
plus = "λm.λn.λf.λx.m f (n f x)"

($$) :: Term -> Term -> Term
e $$ e' = reduce $ App e e'
infixr 1 $$

($.) :: Term -> Term -> Term
($.) = ($$)
infixl 9 $.

toInt :: Term -> Maybe Integer
toInt = getLam . reduce
  where
    getLam e = case e of
      Lam func (Lam v body) -> go func v body
      _                     -> Nothing
    go _ v (Var v') | v == v' = Just (0 :: Integer)
    go func v (App (Var func') n) | func == func' = succ <$> go func v n
    go _ _ _ = Nothing

mult :: Term
mult = "λm.λn.λf.m (n f)"

pow :: Term
pow = "λb.λe.e b"

sub :: Term
sub = fromString $ "λm.λn.n (" <> predd <> ") m"
  where
    predd = "λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)"

liftTerm2 :: Term -> Term -> Term -> Term
liftTerm2 t = ($.) . (t $.)

instance Num Term where
  (+) = liftTerm2 plus
  (*) = liftTerm2 mult
  fromInteger = fromInt
  abs = undefined
  signum = undefined
  (-) = liftTerm2 sub
