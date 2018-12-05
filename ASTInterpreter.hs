module ASTInterpreter where

import Prelude hiding (lookup)
import Data.Map(Map, lookup, insert, empty, fromList)  -- for State



-- Here is the abstract syntax tree for our language
-- Stmts and Expr

type Program = [Stmts]

data Stmts = Def String [String] Stmts 
              | Block [Stmts] 
              | Id Expr 
              | While [Expr] Stmts 
              | If Expr Stmts [Stmts] 
              | Return Expr
              | Print String 
              | Break
              | Continue -- deriving Show

data Expr = String [Expr] 
              | I Integer 
              | B Bool
              | Or Expr Expr
              | And Expr Expr
              | Not Expr
              | Eq Expr Expr
              | Less Expr Expr
              | LessEq Expr Expr
              | Great Expr Expr
              | GreatEq Expr Expr 
              | Plus Expr Expr
              | Minus Expr Expr
              | Mult Expr Expr
              | Div Expr Expr
              | Mod Expr Expr -- deriving Show

data Ast = Stmts | Expr 

instance Show Stmts where
  -- show (Def name params (Block code)) = show "def " ++ name ++ "(" ++ params ++ ") { " ++ (show code) ++ " }" 
  show (Id id) = show id
  show 
  -- show () = undefined
  -- show () = undefined
  -- show () = undefined
  -- show () = undefined
  -- show () = undefined
  show _ = undefined
instance Show Expr where
  show (I i) = show i
  show (B i) = show i
  show (Or x y) = show x ++ " || " ++ show y
  show (And x y) = show x ++ " && " ++ show y
  show (Not x) = "!" ++ show x
  show (Eq x y) = show x ++ " == " ++ show y
  show (Less x y) = show x ++ " < " ++ show y
  show (LessEq x y) = show x ++ " <= " ++ show y
  show (Great x y) = show x ++ " > " ++ show y
  show (GreatEq x y) = show x ++ " >= " ++ show y
  show (Plus x y) = show x ++ " + " ++ show y
  show (Minus x y) = show x ++ " - " ++ show y
  show (Mult x y) = show x ++ " * " ++ show y
  show (Div x y) = show x ++ " / " ++ show y
  show (Mod x y) = show x ++ " % " ++ show y
  show _ = undefined

  -- show Program (x:xs) = "[" ++ show x ++ ", " ++ show xs ++ "]"
  -- show (Def ident params stmts) = "def " ++ show ident ++ "(" ++ show params ++ ") {" 

-- eval :: Ast -> EnvUnsafe Env Val
-- eval (I a) = return $ a
-- -- finish Factor
-- eval (Neg f) = return $ -1 * (eval f)
-- eval (Identifier exp) = undefined


-- -- helper functions that take care of type issues (use a "Error" when things have the wron type
-- evalInt :: Ast -> EnvUnsafe Env Integer
-- evalInt a = do res <- eval a
--                case res of I i -> return i
--                            _ -> err "no int"

-- evalIntZero :: Ast -> EnvUnsafe Env Integer
-- evalIntZero a = do res <- eval a
--                    case res of I 0 -> err "can't divide by 0"
--                                I i -> return i
--                                _ -> err "not an int"


-- evalBool :: Ast -> EnvUnsafe Env Bool
-- evalBool a = do res <- eval a
--                 case res of B i -> return i
--                             _ -> err "no bool"

-- evalList :: Ast -> EnvUnsafe Env [Val]
-- evalList a = do res <- eval a
--                 case res of Ls a -> return a
--                             _ -> err "no lst"

-- evalFun :: Ast -> EnvUnsafe Env (Val -> Unsafe Val)
-- evalFun a = do res <- eval a
--                case res of Fun a -> return a
--                            _ -> err "not a function hehe"

-- -- ungraded bonus challenge: use a helper type class to do this functionality



-- -- This is helpful for testing and debugging
-- fullyParenthesized :: Ast -> String
-- fullyParenthesized (ValInt i) = "(" ++ show i ++ ")"
-- fullyParenthesized (ValBool True) = "(" ++ "true" ++ ")"
-- fullyParenthesized (ValBool False) = "(" ++ "false" ++ ")"
-- fullyParenthesized (And l r) = "(" ++ (fullyParenthesized l) ++ " && " ++ (fullyParenthesized r) ++ ")"
-- fullyParenthesized (Or l r) = "(" ++ (fullyParenthesized l) ++ " || " ++ (fullyParenthesized r) ++ ")"
-- fullyParenthesized (Not a) = "(" ++ " ! " ++ (fullyParenthesized a) ++ ")"
-- fullyParenthesized (Plus l r) = "(" ++ (fullyParenthesized l) ++ " + " ++ (fullyParenthesized r) ++ ")"
-- fullyParenthesized (Minus l r) = "(" ++ (fullyParenthesized l) ++ " - " ++ (fullyParenthesized r) ++ ")"
-- fullyParenthesized (Mult l r) = "(" ++ (fullyParenthesized l) ++ " * " ++ (fullyParenthesized r) ++ ")"
-- fullyParenthesized (Div l r) = "(" ++ (fullyParenthesized l) ++ " / " ++ (fullyParenthesized r) ++ ")"
-- fullyParenthesized (If b t e) = "(if " ++ (fullyParenthesized b) ++ " then " ++ (fullyParenthesized t) ++ " else " ++ (fullyParenthesized e) ++ ")"
-- fullyParenthesized (Let v a bod) = "(let " ++ v ++ " = " ++ (fullyParenthesized a) ++ " in " ++ (fullyParenthesized bod) ++ ")"
-- fullyParenthesized (Lam v bod) = "(\\ " ++ v ++ " -> " ++ (fullyParenthesized bod) ++ ")"
-- fullyParenthesized (App f a) = "( " ++ (fullyParenthesized f)  ++ " " ++ (fullyParenthesized a) ++ ")"
-- fullyParenthesized (Var s) = "( " ++ s ++ ")"
-- fullyParenthesized (Cons h t) = "(" ++ (fullyParenthesized h)  ++ " : " ++ (fullyParenthesized t) ++ ")"
-- fullyParenthesized Nil = "( [] )"


-- showPar :: Bool -> String -> String
-- showPar True s = "(" ++ s ++ ")"
-- showPar false s =  s


-- -- provide a nice show with minimal parentheses, for testing an documentation
-- --the bigger the number the more tight he biding
-- prettyShow :: Ast -> Integer -> String
-- prettyShow (ValInt i) _ =  if i < 0
--                            then  "(" ++ show i ++ ")"
--                            else show i
-- prettyShow (ValBool True) _ =  "true"
-- prettyShow (ValBool False)  _  = "false"
-- prettyShow Nil _ = "[]"
-- prettyShow (Var s) _ = s

-- prettyShow (Lam v bod) i = showPar (i>1) $ "\\ " ++ v ++ " -> " ++ (prettyShow bod 1)
-- prettyShow (Let v a bod)  i = showPar (i>1) $  "let " ++ v ++ " = " ++ (prettyShow a 1) ++ " in " ++ (prettyShow bod 1)
-- prettyShow (If b t e) i = showPar (i>1) $  "if " ++ (prettyShow b 1) ++ " then " ++ (prettyShow t 1) ++ " else " ++ (prettyShow e 1)

-- prettyShow (App l r) i = showPar (i>2) $ (prettyShow l 2) ++ " " ++ (prettyShow r 3)
-- prettyShow (Cons l r) i = showPar (i>4) $ (prettyShow l 5) ++ " : " ++ (prettyShow r 4)
-- prettyShow (Or l r) i = showPar (i>6) $ (prettyShow l 6) ++ " || " ++ (prettyShow r 7)
-- prettyShow (And l r) i = showPar (i>8) $ (prettyShow l 8) ++ " && " ++ (prettyShow r 9)
-- prettyShow (Minus l r) i = showPar (i>10) $ (prettyShow l 10) ++ " - " ++ (prettyShow r 11)
-- prettyShow (Plus l r) i = showPar (i>10) $ (prettyShow l 10) ++ " + " ++ (prettyShow r 11)
-- prettyShow (Mult l r) i = showPar (i>12) $ (prettyShow l 12) ++ " * " ++ (prettyShow r 13)
-- prettyShow (Div l r) i = showPar (i>12) $ (prettyShow l 12) ++ " / " ++ (prettyShow r 13)

-- prettyShow (Not l ) i = showPar (i>14) $  " ! " ++ (prettyShow l 14)


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

