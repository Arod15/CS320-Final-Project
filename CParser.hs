module LangParser where

import ASTInterpreter
import CParserMonad

-- TODO: apps, proofread everything


-- hierarchy of dependency:
-- parse
-- application
-- :
-- orExpr
-- andExpr
-- +, -
-- x, -
-- !
-- atoms

parser :: Parser Ast
parser = apps <||> cons <||> orExpr <||> andExpr <||> addSubExpr <||> multDivExpr <||> notExp <||> atoms

-- note that the parser must respect all the precedence and associativity rules expressed in the prettyShow function.
-- that means
-- ! binds more tightly than
-- * / which binds more tightly than
-- + - which binds more tightly than
-- && which binds more tightly than
-- || which binds more tightly than
-- : which binds more tightly than
-- {the application} which binds weakest of all

-- + - * / && || {the application} are left associative
-- : is right associative

-- we are mostly following the questionable c precedence rules

-- ungraded bonus: add additional pretty syntax for lists: [1,2,3,4]




-- -- some general functions that make parsing this easier
--
oneOf :: [Parser a] -> Parser a
oneOf [] = failParse
oneOf (pa:rest) = pa <||> oneOf rest
--
-- -- *LangParser> parse (oneOf [ints,bools]) "-78"
-- -- Just (-78,"")
-- -- *LangParser> parse (oneOf [ints,bools]) " true"
-- -- Just (true,"")
-- -- *LangParser> parse (oneOf [ints,bools]) " tr ue"
-- -- Nothing
--
--
-- -- we saw before the midterm that there were issues when there are multiple operators with the same precedence, this is a helper function to handle those
-- -- this generalizes the helper function posted on piaza from last time
-- -- it is left associative
withInfix :: Parser a -> [(String, a -> a -> a)] -> Parser a
withInfix pa ls = let operators = fmap fst ls
                      opParsers = fmap (\ s -> token $ literal s) operators

                      --innerParser :: a -> Parser a, where a is the same as above
                      innerParser left = do s <- oneOf opParsers
                                            next <- pa
                                            case lookup s ls of
                                             Nothing -> failParse
                                             Just f ->  let out = f left next
                                                        in (innerParser out) <||> return out
                  in do l <- pa
                        (innerParser l) <||> (return l)
--
-- -- *LangParser> parse (withInfix intParser [("+", (+)), ("-", (-))]) "1+2+3+4-5"
-- -- Just (5,"")
-- -- *LangParser> parse (withInfix intParser [("+", (+)), ("-", (-))]) "100-1-10"
-- -- Just (89,"")
--
--
-- -- you may want to structure you grammar like this:
--
keywords = ["if","then","else", "let", "in", "true","false"]
--
--
vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

ints :: Parser Ast
ints = do s <- token $ intParser
          return $ ValInt s

bools :: Parser Ast
bools = do s <- token $ literal "true" <||> literal "false"
           if s == "true"
           then return $ ValBool True
           else return $ ValBool False

nil :: Parser Ast
nil = do s <- (token $ literal "[]")
         return $ Nil


apps :: Parser Ast
apps = withInfix cons [("",App)] -- the tokens eat up all the spaces so we split on the empty string

-- hierarchy of dependency:
-- parse
-- application
-- :
-- orExpr
-- andExpr
-- +, -
-- x, -
-- !
-- atoms

cons :: Parser Ast
cons = (do s <- orExpr -- maintain the hierarchy of dependency
           token $ literal ":"
           t <- cons
           return $ Cons s t) <||> orExpr

-- *LangParser> parse cons "1 : 4: true"
-- Just (1 : 4 : true,"")
-- *LangParser> parse cons "1 : 4: 3+5"
-- Just (1 : 4 : (3 + 5),"")


orExpr :: Parser Ast
orExpr = withInfix andExpr [("||", Or)]

-- *LangParser> parse orExpr "true || false && 7"
-- Just (true || false && 7,"")
-- *LangParser> parse orExpr "true || false || 7"
-- Just (true || false || 7,"")
-- *LangParser> parse orExpr "true"
-- Just (true,"")

andExpr :: Parser Ast
andExpr = withInfix addSubExpr [("&&", And)]

-- *LangParser> parse andExpr "false"
-- Just (false,"")
-- *LangParser> parse andExpr "false && false"
-- Just (false && false,"")

addSubExpr :: Parser Ast
addSubExpr = withInfix multDivExpr [("+", Plus), ("-", Minus)]

-- *LangParser> parse addSubExpr "1+2+3+4"
-- Just (1 + 2 + 3 + 4,"")
-- *LangParser> parse addSubExpr "1-2-3-4"
-- Just (1 - 2 - 3 - 4,"")

multDivExpr :: Parser Ast
multDivExpr = withInfix notExp [("*", Mult), ("/", Div)]

notExp :: Parser Ast
notExp = (do token $ literal "!"
             s <- notExp
             return $ Not s) <||> atoms


atoms:: Parser Ast
atoms = ints <||> bools  <||>  nil <||> parens <||> ifParser <||> letParser <||>  lambdaParser <||> vars

-- *LangParser> parse atoms "111"
-- Just (111,"")
-- *LangParser> parse atoms "  true"
-- Just (true,"")

-- THESE NEXT ONES ARE RECURSIVE

ifParser :: Parser Ast
ifParser = do token $ literal "if"
              cond <- token $ parser
              token $ literal "then"
              first <- token $ parser
              token $ literal "else"
              sec <- token $ parser
              return $ If cond first sec

letParser :: Parser Ast
letParser = do token $ literal "let"
               var <- token $ varParser
               token $ literal "="
               value <- token $ parser
               token $ literal "in"
               expression <- token $ parser
               return $ Let var value expression


-- *LangParser> parse letParser "let x=3 in x+x"
-- Just (let x = 3 in x + x,"")



lambdaParser :: Parser Ast
lambdaParser = do token $ literal "\\"
                  var <- token $ varParser
                  token $ literal "->"
                  body <- token $ parser
                  return $ Lam var body


parens :: Parser Ast
parens = do token $ literal "("
            s <- token $ parser
            token $ literal ")"
            return $ s

-- *LangParser> parse parser "(true)"
-- Just (true,"")
-- *LangParser> parse parser "let x = (if true and false then 3 else elsee) in x + x"
-- Just (let x = if true and false then 3 else elsee in x + x,"")




-- Some examples of weird stuff

-- ex = prettyShow  (Mult (Var "a") (Or (Var "b") (Var "c"))) 0

-- ex1 = prettyShow (Minus (Var "y") (Minus (App (ValBool True) (ValInt (-3))) (Mult (ValBool False) (ValBool False)))) 0

-- ex2 = prettyShow (Cons (Var "z") (Not (Not (Plus (Mult (ValInt (-18)) Nil) (Not (ValInt 2)))))) 0

-- ex3 = "! ! (-18)"