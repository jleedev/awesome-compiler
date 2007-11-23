module Program where

type ID = String
type Dimension = Integer

type TypeNum = Integer
type TypeReal = Double

data Program = Program Block deriving Show
data Block = Block [Decl] [Stmt] deriving Show

data Decl = Decl Type ID deriving Show
data Type = Type BasicType [Dimension] deriving Show
data BasicType = BasicInt | BasicFloat deriving Show

data Stmt = StmtAssign  Loc Expr
          | StmtIf      Expr Stmt
          | StmtIfElse  Expr Stmt Stmt
          | StmtWhile   Expr Stmt
          | StmtDoWhile Stmt Expr
          | StmtBreak
          | StmtEmpty
          | StmtBlock   Block
          deriving Show

data Loc = LocIndex  Loc Expr
         | LocScalar ID
         deriving Show

data Expr = BinExpr Op Expr Expr
          | UnExpr  Op Expr
          | Loc
          | LitNum TypeNum
          | LitReal TypeReal
          | LitBool Bool
          deriving Show

data Op = OpAdd | OpSub | OpMul | OpDiv
        | OpLT  | OpGT  | OpEQ  | OpNE | OpGE | OpLE
        | OpAnd | OpOr  | OpNot
        | OpAssign
        deriving Show
