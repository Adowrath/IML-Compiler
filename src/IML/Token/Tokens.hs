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
    | Ident String
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

keywordList :: [(String, Token)]
keywordList = 
    [ ("bool",       Type BoolType   )
    , ("call",       Call            )
    , ("const",      ChangeMode Const)
    , ("copy",       MechMode Copy   )
    , ("debugin",    DebugIn         )
    , ("debugout",   DebugOut        )
    , ("divE",       Operator DivE   )
    , ("do",         Do              )
    , ("else",       Else            )
    , ("endfun",     Endfun          )
    , ("endif",      Endif           )
    , ("endproc",    Endproc         )
    , ("endprogram", Endprogram      )
    , ("endwhile",   Endwhile        )
    , ("false",      BoolLit False   )
    , ("fun",        Fun             )
    , ("global",     Global          )
    , ("if",         If              )
    , ("in",         FlowMode In     )
    , ("init",       Init            )
    , ("inout",      FlowMode InOut  )
    , ("int64",      Type Int64Type  )
    , ("local",      Local           )
    , ("modE",       Operator ModE   )
    , ("not",        Operator Not    )
    , ("out",        FlowMode Out    )
    , ("proc",       Proc            )
    , ("program",    Program         )
    , ("ref",        MechMode Ref    )
    , ("returns",    Returns         )
    , ("skip",       Skip            )
    , ("then",       Then            )
    , ("true",       BoolLit True    )
    , ("var",        ChangeMode Var  )
    , ("while",      While           )
    ]