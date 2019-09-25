module Tokens where

type TokenList = [Token]

data Token =
    Operator OpType
    | LParen | RParen
    | Comma | Semicolon | Colon
    | Init
    | Becomes -- :=

    | FlowMode FlowMode
    | MechMode MechMode
    | ChangeMode ChangeMode

    | Type Type
    | BoolLit Bool

    | Call
    | Fun | Endfun
    | If | Then | Else | Endif
    | Proc | Endproc
    | Progarm | Endprogram
    | While | Do | Endwhile

    | DebugIn | DebugOut
    | Global | Local
    | Returns | Skip
    deriving Show

{-
DivE/ModE is Euclidian
*F is Floored
*T is Truncated

And vs. CAnd? We do not know.
-}
data OpType =
    Plus | Minus | Times
    | DivE | ModE -- | DvF | ModF | DivT | ModT
    | LT | GT | LTE | GTE | EQ | NE
    | Not -- | And | Or | CAnd | COr
    deriving Show

data FlowMode = In | InOut | Out deriving Show
data MechMode = Copy | Ref deriving Show
data ChangeMode = Const | Var deriving Show
data Type = BoolType | Int64Type deriving Show
