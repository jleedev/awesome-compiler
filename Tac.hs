module Tac (
    Tac (..),
    Arg (..),
    Label (..),
    Instr (..)
) where

import Program

data Tac = Tac { instr  :: Instr
               , arg1   :: Maybe Arg
               , arg2   :: Maybe Arg
               , result :: Arg
               } deriving Show

data Arg = ArgNum   TypeNum
         | ArgReal  TypeReal
         | ArgID    ID
         | ArgLabel Label
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
           deriving Show
