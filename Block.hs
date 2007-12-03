module Block (
    Tree,
    Symbol,
    SymbolTable, modifySymbolTable,
    CompilerState,
    newCompilerState,
    addLabel, getLabel,
    addTemp, getTemp,
    addDecl
) where

import qualified Data.Map as Map
import Debug.Trace (trace)

import Program
import Tac

data Tree a = Branch a [Tree a] deriving Show

data Context a = Context { parent :: Maybe (Context a)
                         , left   :: [Tree a]
                         , right  :: [Tree a]
                         , this   :: Tree a } deriving Show

data Symbol = Symbol { isTemp  :: Bool
                     , symKind :: Type
                     , symAddr :: Int } deriving Show

type SymbolTable = Map.Map ID Symbol

-- before we parse the body of a do or while loop, we set breakPoint to
-- the label where break statements should jump to.
data CompilerState = CompilerState { symbolTable :: Context SymbolTable
                                   , breakPoint  :: Maybe Label
                                   , nextLabel   :: Label
                                   , nextTemp    :: Int } deriving Show

newCompilerState = CompilerState { symbolTable = newSymbolTable
                                 , breakPoint  = Nothing
                                 , nextLabel   = Label 0
                                 , nextTemp    = 0 }

newSymbolTable :: Context SymbolTable
newSymbolTable = Context { parent = Nothing
                         , left = []
                         , right = []
                         , this = Branch Map.empty [] }

activationRecord :: SymbolTable -> Int
activationRecord = Map.fold (\a s -> getSize (symKind a) + s) 0

openBlock :: CompilerState -> CompilerState
openBlock state@CompilerState { symbolTable = Context { this = st } } =
    undefined

addLabel :: CompilerState -> CompilerState
addLabel state@CompilerState { nextLabel = Label n } =
    state { nextLabel = Label (n+1) }

getLabel = nextLabel

addTemp :: BasicType -> CompilerState -> CompilerState
addTemp typ state@CompilerState { nextTemp = n } =
    state { nextTemp = n+1
          , symbolTable = symbolTable $ addDecl ("_t" ++ show n) True (Type typ []) state }

getTemp = ("_t" ++) . show . nextTemp

resetTemp :: CompilerState -> CompilerState
resetTemp state = state { nextTemp = 0 }

addDecl :: ID ->   -- ^ symbol name
           Bool -> -- ^ is temporary
           Type -> -- ^ kind of variable
           CompilerState -> CompilerState
addDecl i tmp typ = modifySymbolTable $
    Map.insert i Symbol { isTemp = tmp, symKind = typ, symAddr = 0 }

modifySymbolTable :: (SymbolTable -> SymbolTable) -> CompilerState -> CompilerState
modifySymbolTable f state@CompilerState { symbolTable = c@Context { this = Branch st sub } } = state { symbolTable = c { this = Branch (f st) sub } }
