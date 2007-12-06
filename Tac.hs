module Tac (
    Tac (..),
    Arg (..), Env,
    Label (..),
    Instr (..)
) where

import Program (TypeNum, TypeReal, ID, Op)

data Tac = Tac { instr  :: Instr
               , arg1   :: Maybe Arg
               , arg2   :: Maybe Arg
               , result :: Arg
               } deriving Show

type Env = Int

data Arg = ArgNum   TypeNum
         | ArgReal  TypeReal
         | ArgID    ID
         | ArgLabel Label
         | ArgEnv   Env
         deriving Show

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
           deriving Show
