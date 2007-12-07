-- |Three address code representation. Includes data structures to hold
-- instructions, arguments to the instructions, and the types of
-- declarations.
module Tac where

-- |Identifiers are keyed as strings.
type ID = String
-- |The size of an array. Must be positive.
type Dimension = Integer

-- |The generated code uses Integer and Double to represent its types.
data BasicType = BasicInt | BasicFloat
instance Show BasicType where
    show BasicInt = "int"
    show BasicFloat = "float"

type TypeNum = Integer
type TypeReal = Double

-- |A Type is a combination of two things: a BasicType and a list of
-- Dimensions expressing its size.
data Type = Type BasicType [Dimension]
instance Show Type where
    show (Type b d) = show b ++ foldr (\h t -> "[" ++ show h ++ "]" ++ t) "" d

-- |The size of a basic type is 4 for an Integer and 8 for a Double. The
-- size of an array is the size of its basic type times the product of
-- its dimensions.
getSize :: Type -> Int
getSize (Type bt dim) = fromIntegral (product dim) * s bt
    where s BasicInt = 4
          s BasicFloat = 8

-- |Three address code. It contains an instruction code, up to two
-- arguments, and a result.
data Tac = Tac { instr  :: Instr
               , arg1   :: Maybe Arg
               , arg2   :: Maybe Arg
               , result :: Maybe Arg
               } deriving Show

-- |A unique key referring to a symbol table environment.
type Env = Int

-- |An argument can either be a literal int, a literal float, a
-- variable (temporary or explicit), a label to jump to, or a symbol
-- table to switch to.
data Arg = ArgNum   TypeNum
         | ArgReal  TypeReal
         | ArgID    ID
         | ArgLabel Label
         | ArgEnv   Env
         deriving Show

-- |When a variable is indexed at a location, an ArrayIndex is
-- constructed. Code using these is currently not implemented.
data ArrayIndex = ArrayIndex ID [Arg] deriving Show

-- |A unique label to jump to.
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
