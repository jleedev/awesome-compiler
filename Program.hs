module Program where

type ID = String
type Dimension = Integer

type TypeNum = Integer
type TypeReal = Double

data Program = Program Block deriving Show
data Block = Block [Decl] [Stmt] deriving Show

data Decl = Decl Type ID
instance Show Decl where
    show (Decl t i) = show t ++ " " ++ i ++ ";"

data Type = Type BasicType [Dimension]
instance Show Type where
    show (Type b d) = show b ++ foldr (\h t -> "[" ++ show h ++ "]" ++ t) "" d

data BasicType = BasicInt | BasicFloat
instance Show BasicType where
    show BasicInt = "int"
    show BasicFloat = "float"

data Stmt = StmtAssign  Loc Expr
          | StmtIf      Expr Stmt
          | StmtIfElse  Expr Stmt Stmt
          | StmtWhile   Expr Stmt
          | StmtDoWhile Stmt Expr
          | StmtBreak
          | StmtEmpty
          | StmtBlock   Block
          deriving Show

data Loc = LocIndex ID [Expr]
         deriving Show

data Expr = BinExpr Op Expr Expr
          | UnExpr  Op Expr
          | LocExpr Loc
          | LitNum TypeNum
          | LitReal TypeReal
          | LitBool Bool
          deriving Show

data Op = OpAdd | OpSub | OpMul | OpDiv
        | OpLT  | OpGT  | OpEQ  | OpNE | OpGE | OpLE
        | OpAnd | OpOr  | OpNot | OpNeg
        deriving Show
