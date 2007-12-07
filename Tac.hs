module Tac where

type ID = String
type Dimension = Integer

type TypeNum = Integer
type TypeReal = Double

data Type = Type BasicType [Dimension]
instance Show Type where
    show (Type b d) = show b ++ foldr (\h t -> "[" ++ show h ++ "]" ++ t) "" d

getSize :: Type -> Int
getSize (Type bt dim) = fromIntegral (product dim) * s bt
    where s BasicInt = 4
          s BasicFloat = 8

data BasicType = BasicInt | BasicFloat
instance Show BasicType where
    show BasicInt = "int"
    show BasicFloat = "float"

data Tac = Tac { instr  :: Instr
               , arg1   :: Maybe Arg
               , arg2   :: Maybe Arg
               , result :: Maybe Arg
               } deriving Show

type Env = Int

data Arg = ArgNum   TypeNum
         | ArgReal  TypeReal
         | ArgID    ID
         | ArgLabel Label
         | ArgEnv   Env
         deriving Show

data ArrayIndex = ArrayIndex ID [Arg] deriving Show

newtype Label = Label Int deriving Show

data Instr = InstrBinary Op
           | InstrUnary Op
           | InstrAssign
           | InstrIf
           | InstrIfNot
           | InstrGoto
           | InstrLabel
           | InstrSet
           | InstrGet
           | InstrEnv
           | InstrNoop
           deriving Show

data Op = OpAdd | OpSub | OpMul | OpDiv
        | OpLT  | OpGT  | OpEQ  | OpNE | OpGE | OpLE
        | OpAnd | OpOr  | OpNot | OpNeg
        deriving Show

noop = Tac InstrNoop Nothing Nothing Nothing
