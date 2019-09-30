module IML.Token.Tokens where

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
    | Program | Endprogram
    | While | Do | Endwhile

    | DebugIn | DebugOut
    | Global | Local
    | Returns | Skip
    deriving (Show, Eq)

{-
DivE/ModE is Euclidian
*F is Floored
*T is Truncated

And vs. CAnd? We do not know.
-}
data OpType =
    Plus | Minus | Times
    | DivE | ModE -- DvF | ModF | DivT | ModT
    | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals | Equals | NotEquals
    | Not -- And | Or | CAnd | COr
    deriving (Show, Eq)

data FlowMode = In | InOut | Out deriving (Show, Eq)
data MechMode = Copy | Ref deriving (Show, Eq)
data ChangeMode = Const | Var deriving (Show, Eq)
data Type = BoolType | Int64Type deriving (Show, Eq)
