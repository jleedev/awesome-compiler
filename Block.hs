module Block (
    Tree,
    Symbol,
    SymbolTable,
    CompilerState
) where

import qualified Data.Map as M

import Program
import Tac

data Tree a = Branch a [Tree a] deriving (Eq, Show)

data Symbol = Symbol { isTemp  :: Bool
                     , symKind :: Type
                     , symAddr :: Int } deriving Show

type SymbolTable = M.Map ID Symbol

data CompilerState = CompilerState { symbolTable :: Maybe (Tree SymbolTable)
                                   , breakPoint  :: Label
                                   , nextLabel   :: Label
                                   , nextTemp    :: Int } deriving Show

newSymbolTable :: Tree SymbolTable
newSymbolTable = Branch M.empty []

appendChild :: Tree a -> Tree a -> Tree a
appendChild child parent@(Branch x y) = Branch x (y ++ [child])

newLabel :: ...

addDecl :: ID ->   -- ^ symbol name
           Bool -> -- ^ is temporary
           Type -> -- ^ kind of variable
           CompilerState -> CompilerState
addDecl i tmp typ state@CompilerState { symbolTable = Just (Branch s sub) } =
    let sy = Symbol { isTemp = tmp, kind = typ, addr = 0 }
        s' = M.insert i sy s
        in state { symbolTable = Just (Branch s' sub) }
