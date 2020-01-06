module IML.Token.Tokens where

-- |
-- Tokens are the smallest useful units of IML.
data Token =
    Operator OpType
    | LParen | RParen
    | CondOpr
    | Comma | Semicolon | Colon
    | Init
    | Becomes -- :=

    | FlowMode FlowMode
    | MechMode MechMode
    | ChangeMode ChangeMode

    | Type Type
    | Ident String
    | IntLit Integer
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
DivE/ModE is Euclidean
*F is Floored
*T is Truncated
-}
-- |
-- The different types of operators that IML supports.
data OpType =
    Plus | Minus | Times
    | DivE | ModE | DivF | ModF | DivT | ModT
    | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals | Equals | NotEquals
    | Not {- And | Or -} | CAnd | COr
    deriving (Show, Eq)

-- | The Flowmode decides the type of data flow for function parameters.
data FlowMode = In | InOut | Out deriving (Show, Eq)
-- | TODO: I don't quite know that yet.
data MechMode = Copy | Ref deriving (Show, Eq)
-- | Defines whether a variable is a constant or mutable.
data ChangeMode = Const | Var deriving (Show, Eq)
-- | The different types of primitives IML supports.
data Type = BoolType | Int32Type | Int64Type | Int1024Type deriving (Show, Eq)

-- | A simple lookup list of all predefined keywords.
keywordList :: [(String, Token)]
keywordList =
    [ ("bool",       Type BoolType   )
    , ("call",       Call            )
    , ("const",      ChangeMode Const)
    , ("copy",       MechMode Copy   )
    , ("debugin",    DebugIn         )
    , ("debugout",   DebugOut        )
    , ("divE",       Operator DivE   )
    , ("divF",       Operator DivF   )
    , ("divT",       Operator DivT   )
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
    , ("int32",      Type Int32Type  )
    , ("int64",      Type Int64Type  )
    , ("int1024",    Type Int1024Type)
    , ("local",      Local           )
    , ("modE",       Operator ModE   )
    , ("modF",       Operator ModF   )
    , ("modT",       Operator ModT   )
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
