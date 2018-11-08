module MiniLogo where

import Prelude hiding (Num)
import Data.List
type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down 
          | Up
 deriving (Eq, Show)

data Expr = VarExp Var
          | Lit Num
          | Addtwo Expr Expr
 deriving (Eq, Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
 deriving (Eq, Show)

-- define line (x1,y1,x2,y2) {
-- pen up; move(x1,y1);
-- pen down; move(x2,y2);
-- }
line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] [Pen Up, Move (VarExp "x1") (VarExp "y1"), Pen Down, Move (VarExp "x2") (VarExp "y2")]

--define nix (x, y , w, h) {
--line(x, y, (x + w) , (y + h));
--line((x + w), y, x, (y + h));
--}

nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"] [Call "line" [VarExp"x", VarExp"y", (Addtwo (VarExp "x") (VarExp "w")), (Addtwo (VarExp "y") (VarExp "h"))], Call "line" [(Addtwo (VarExp "x") (VarExp "w")), VarExp "y", VarExp "x", (Addtwo (VarExp "y") (VarExp "h"))]]

steps :: Int -> Prog
steps 0 = []
steps 1 = [Call "line" [Lit 0, Lit 0, Lit 1, Lit 1]]
steps x = steps (x - 1) ++ [Call "line" [Lit x, Lit x, Lit (x), Lit (x + 1)], Call "line" [Lit x, Lit (x + 1), Lit (x + 1), Lit (x + 1)]] 

macros :: Prog -> [Macro]
macros [] = []
macros ((Move _ _) : cmdlist) = macros cmdlist
macros ((Pen _) : cmdlist) = macros cmdlist
macros ((Define x _ p) : cmdlist)  = x : macros cmdlist ++ macros p
macros ((Call x _) : cmdlist) = x : macros  cmdlist


pretty :: Prog -> String 
pretty [] = " "
pretty n = concat(map prettyCmd n)

-- map takes in function prettyCmd and List n 
-- map goes through every cmd in list n and pretty cmds it
-- outputs a new list that is all the pretty printed strings for each command in that list
-- that list is then concatanated into one string that is pretty printed 

prettyCmd :: Cmd -> String
prettyCmd (Pen Up)   = "pen up" ++ ";\n"
prettyCmd (Pen Down) = "pen down" ++ ";\n"
prettyCmd (Move a b) = "move" ++ " (" ++ prettyExpr a ++ ", " ++ prettyExpr b ++ ")" ++ ";\n"
prettyCmd (Define mac v pro) = "Define" ++ " " ++ mac ++  " " ++ "(" ++ concat(intersperse ", " v) ++ ")" ++ "{\n" ++ pretty pro ++ "}"
prettyCmd (Call mac expList) = mac ++ " " ++ "(" ++ concat(intersperse ", " (map prettyExpr expList)) ++ ")" ++ ";\n"

prettyExpr :: Expr -> String
prettyExpr (VarExp a) = a
prettyExpr (Lit a) = show(a)
prettyExpr (Addtwo a b) = prettyExpr(a) ++ " " ++ prettyExpr(b)
