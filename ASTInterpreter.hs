module ASTInterpreter where

import Prelude hiding (lookup)
import Data.Map(Map, lookup, insert, empty, fromList)  -- for State

import CEnvUnsafe


-- Here is the abstract syntax tree for our language

data Term = F Factor | Mult Term Factor | Div Term Factor | Mod Term Factor

data Expr = T Term | Plus Expr Term | Minus Expr Term

data Identifier = S String | Exp Expr

data Factor = I Integer | Neg Factor | Identifier Expr

---------

data Cond = Equals Expr Expr 
            | NotEquals Expr Expr 
            | LessThan Expr Expr 
            | LessThanEquals Expr Expr 
            | GreaterThan Expr Expr 
            | GreaterThanEquals Expr Expr

---------

data BFactor = C Cond | Not BFactor | BExpr

data BTerm = BF BFactor | And BFactor BTerm

data BExpr = BT BTerm | Or BTerm BExpr

---------

data Stmt = Assign String Expr | Return Expr | Print Identifier | Break | Continue

data Block = ListStmts [Stmt] | While BExpr Block | If BExpr Block | IfElse BExpr Block Block

data Stmts = Statement Stmt | StmtList [Stmt] | BS Block Stmts | Bl Block 

data Func = DefParams Identifier Identifier Stmts | DefNoParams Identifier Stmts

data Funcs = Fn Func | FnList [Func]

type Program = Funcs

---------

data Ast = ValInt Int
         | Plus Ast Ast | Minus Ast Ast | Times Ast Ast | Div Ast Ast | Mod Ast Ast
         
         | ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast

         | Nil
         | Cons Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast
         
        --  deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq

instance Show Ast where
  show ast = prettyShow ast 0


-- the goal of the program is to return a value
data Val = I Integer | B Bool
         | Ls [Val]
         | Fun (Val -> Unsafe Val) -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (Ls ls) = show ls
  show (Fun _) = "->" -- no good way to show a function

-- BARC
stdLib = fromList [("tail", Fun $ \ v -> case v of Ls (_:ls)-> Ok $ Ls ls
                                                   _ -> Error "not an non empty list"),
                   ("head", Fun $ \v -> case v of Ls (l:ls) -> Ok l
                                                  _ -> Error "not something"),
                   ("len", Fun $ \v -> case v of Ls lst -> Ok $ I $ foldr (\_ n -> 1 + n) 0 lst
                                                 _ -> Error "not something")]
-- stdLib = undefined

-- helper function that runs with a standard library of functions: head, tail ...
run :: Ast -> (Unsafe Val)
run a = r (eval a) stdLib


type Env = Map String Val


eval :: Ast -> EnvUnsafe Env Val
eval (ValBool a) = return $ B $ a
eval (And a b) = do n <- evalBool a
                    m <- evalBool b
                    return $ B $ n && m
eval (Or a b) = do n <- evalBool a
                   m <- evalBool b
                   return $ B $ n || m
eval (Not a) = do n <- evalBool a
                  return $ B $ not n

eval (ValInt a) = undefined
eval (Plus a b) = do n <- evalInt a
                     m <- evalInt b
                     return $ I $ n + m
eval (Minus a b) = do n <- evalInt a
                      m <- evalInt b
                      return $ I $ n - m
eval (Mult a b) = undefined
eval (Div a b) = do n <- evalInt a
                    m <- evalInt b
                    case m of 0 -> err "fuck me"
                              i -> return $ I $ n `div` i

eval (Nil) = return $ Ls []
eval (Cons a b) = do n <- eval a
                     m <- evalList b
                     return $ Ls (n:m)

eval (If a b c) = do a' <- evalBool a
                     b' <- eval b
                     c' <- eval c
                     if a' == True
                     then return b'
                     else return c'
eval (Let var val bod) = eval $ App (Lam var bod) val 
  
  -- do val' <- eval val
  --                           withVal var val' (eval bod)
eval (Var str) = valOf str
-- eval (Var str) = do env <- getEnv
--                     valOf str
eval (Lam var body) = do env <- getEnv
                         return $ Fun $ \v -> r (eval body) (insert var v env)
eval (App f a) = do a' <- eval a
                    f' <- evalFun f -- f' is type sign val -> unsafe val
                    case f' a' of Ok a -> return a
                                  Error s -> err s


-- Let variable name = variable value in expression
-- withVal Implementation
-- do val' <- eval val
--     withVal var val' (eval bod)
-- eval $ app (lam var bod) val


-- some examples
-- example = let x = Var "x"
--           in run $ App (Lam "x" ( x `Plus` x))  (ValInt 7)
--
-- example2 = let x = Var "x"; y = Var "y"
--            in run $ ((Lam "x" (Lam "y" ( x `Plus` y))) `App` (ValInt 7)) `App` (ValInt 4)

-- some helper function, you may find helpful
valOf :: String -> EnvUnsafe Env Val
valOf var = do env <- getEnv
               case (lookup var env) of Nothing -> err "variable doesn't exist"
                                        Just a -> return a
--
-- -- add a val into the environment
withVal :: String -> Val -> EnvUnsafe Env a -> EnvUnsafe Env a
withVal var v comp = do env <- getEnv
                        let env' = insert var v env in
                          case r comp env of
                            Ok a -> return a
                            Error s -> err s


-- helper functions that take care of type issues (use a "Error" when things have the wron type
evalInt :: Ast -> EnvUnsafe Env Integer
evalInt a = do res <- eval a
               case res of I i -> return i
                           _ -> err "no int"

evalIntZero :: Ast -> EnvUnsafe Env Integer
evalIntZero a = do res <- eval a
                   case res of I 0 -> err "can't divide by 0"
                               I i -> return i
                               _ -> err "not an int"


evalBool :: Ast -> EnvUnsafe Env Bool
evalBool a = do res <- eval a
                case res of B i -> return i
                            _ -> err "no bool"

evalList :: Ast -> EnvUnsafe Env [Val]
evalList a = do res <- eval a
                case res of Ls a -> return a
                            _ -> err "no lst"

evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
evalFun a = do res <- eval a
               case res of Fun a -> return a
                           _ -> err "not a function hehe"

-- ungraded bonus challenge: use a helper type class to do this functionality



-- This is helpful for testing and debugging
fullyParenthesized :: Ast -> String
fullyParenthesized (ValInt i) = "(" ++ show i ++ ")"
fullyParenthesized (ValBool True) = "(" ++ "true" ++ ")"
fullyParenthesized (ValBool False) = "(" ++ "false" ++ ")"
fullyParenthesized (And l r) = "(" ++ (fullyParenthesized l) ++ " && " ++ (fullyParenthesized r) ++ ")"
fullyParenthesized (Or l r) = "(" ++ (fullyParenthesized l) ++ " || " ++ (fullyParenthesized r) ++ ")"
fullyParenthesized (Not a) = "(" ++ " ! " ++ (fullyParenthesized a) ++ ")"
fullyParenthesized (Plus l r) = "(" ++ (fullyParenthesized l) ++ " + " ++ (fullyParenthesized r) ++ ")"
fullyParenthesized (Minus l r) = "(" ++ (fullyParenthesized l) ++ " - " ++ (fullyParenthesized r) ++ ")"
fullyParenthesized (Mult l r) = "(" ++ (fullyParenthesized l) ++ " * " ++ (fullyParenthesized r) ++ ")"
fullyParenthesized (Div l r) = "(" ++ (fullyParenthesized l) ++ " / " ++ (fullyParenthesized r) ++ ")"
fullyParenthesized (If b t e) = "(if " ++ (fullyParenthesized b) ++ " then " ++ (fullyParenthesized t) ++ " else " ++ (fullyParenthesized e) ++ ")"
fullyParenthesized (Let v a bod) = "(let " ++ v ++ " = " ++ (fullyParenthesized a) ++ " in " ++ (fullyParenthesized bod) ++ ")"
fullyParenthesized (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (fullyParenthesized bod) ++ ")"
fullyParenthesized (App f a) = "( " ++ (fullyParenthesized f)  ++ " " ++ (fullyParenthesized a) ++ ")"
fullyParenthesized (Var s) = "( " ++ s ++ ")"
fullyParenthesized (Cons h t) = "(" ++ (fullyParenthesized h)  ++ " : " ++ (fullyParenthesized t) ++ ")"
fullyParenthesized Nil = "( [] )"


showPar :: Bool -> String -> String
showPar True s = "(" ++ s ++ ")"
showPar false s =  s


-- provide a nice show with minimal parentheses, for testing an documentation
--the bigger the number the more tight he biding
prettyShow :: Ast -> Integer -> String
prettyShow (ValInt i) _ =  if i < 0
                           then  "(" ++ show i ++ ")"
                           else show i
prettyShow (ValBool True) _ =  "true"
prettyShow (ValBool False)  _  = "false"
prettyShow Nil _ = "[]"
prettyShow (Var s) _ = s

prettyShow (Lam v bod) i = showPar (i>1) $ "\\ " ++ v ++ " -> " ++ (prettyShow bod 1)
prettyShow (Let v a bod)  i = showPar (i>1) $  "let " ++ v ++ " = " ++ (prettyShow a 1) ++ " in " ++ (prettyShow bod 1)
prettyShow (If b t e) i = showPar (i>1) $  "if " ++ (prettyShow b 1) ++ " then " ++ (prettyShow t 1) ++ " else " ++ (prettyShow e 1)

prettyShow (App l r) i = showPar (i>2) $ (prettyShow l 2) ++ " " ++ (prettyShow r 3)
prettyShow (Cons l r) i = showPar (i>4) $ (prettyShow l 5) ++ " : " ++ (prettyShow r 4)
prettyShow (Or l r) i = showPar (i>6) $ (prettyShow l 6) ++ " || " ++ (prettyShow r 7)
prettyShow (And l r) i = showPar (i>8) $ (prettyShow l 8) ++ " && " ++ (prettyShow r 9)
prettyShow (Minus l r) i = showPar (i>10) $ (prettyShow l 10) ++ " - " ++ (prettyShow r 11)
prettyShow (Plus l r) i = showPar (i>10) $ (prettyShow l 10) ++ " + " ++ (prettyShow r 11)
prettyShow (Mult l r) i = showPar (i>12) $ (prettyShow l 12) ++ " * " ++ (prettyShow r 13)
prettyShow (Div l r) i = showPar (i>12) $ (prettyShow l 12) ++ " / " ++ (prettyShow r 13)

prettyShow (Not l ) i = showPar (i>14) $  " ! " ++ (prettyShow l 14)


--some more examples:

-- e1 = prettyShow (Minus (ValInt 100) (Minus (ValInt 2) (ValInt 5))) $  0
-- e2 = prettyShow (Minus (Minus (ValInt 100) (ValInt 2)) (ValInt 5) ) $  0
--
--
-- e3 = prettyShow (Minus (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
-- e4 = prettyShow (Div (Minus (ValInt 100) (ValInt 2)) (Div (Div (ValInt 100) (ValInt 2)) (ValInt 5) )  ) $ 0
-- e5 = prettyShow (((Var "fun") `App` (ValInt 2)) `App` (ValInt 5)) $ 0
--
-- e6 = prettyShow (Not $ Not $ ((Var "fun") `App` (ValInt 2)) `App` (Not $ ValInt 5)) $ 0

