-- |
-- All the data structures stored during the compilation are represented
-- here. Apart from the bookkeeping devices in 'CompilerState', the main
-- purpose is a graph of 'SymbolTable' objects, allowing identifiers to
-- be looked up in the current and parent scopes.

module Block (
    -- | Oh yeah.
    Symbol(..),
    Env,
    SymbolTable, modifySymbolTable,
    Scope(..),
    Blocks,
    CompilerState(..), newCompilerState,
    activationRecord,
    openBlock, closeBlock, getLocals,
    addLabel,
    addTemp, getTemp,
    addDecl
) where

import qualified Data.Map as Map

import Tac

-- | A symbol.
data Symbol = Symbol {
    isTemp  :: Bool, -- ^ is this symbol temporary?
    symType :: Type,
    symAddr :: Int
}
instance Show Symbol where
    show s = concat [
        if isTemp s then "temporary " else "",
        show $ symType s,
        " at ",
        show $ symAddr s]

type SymbolTable = Map.Map ID Symbol

data Scope = Scope {
    table :: SymbolTable,
    parent :: Env
} deriving Show

type Blocks = Map.Map Env Scope

data CompilerState = CompilerState {
    blocks :: Blocks,
    locals :: Env,
    nextEnv :: Env,
    breakPoint :: Maybe Label,
    nextLabel :: Label,
    nextTemp :: Int,
    code :: [Tac]
} deriving Show

newCompilerState = CompilerState {
    blocks = Map.empty,
    locals = 0,
    nextEnv = 0,
    breakPoint = Nothing,
    nextLabel = Label 0,
    nextTemp = 0,
    code = []
}

activationRecord :: Scope -> Int
activationRecord = (Map.fold (\a s -> getSize (symType a) + s) 0) . table

addLabel :: CompilerState -> CompilerState
addLabel state@CompilerState { nextLabel = Label n } =
    state { nextLabel = Label (n+1) }

addTemp :: BasicType -> CompilerState -> CompilerState
addTemp typ state = state' { nextTemp = n + 1 }
    where state' = addDecl ("_t" ++ show n) True (Type typ []) state
          n = nextTemp state

getTemp = ("_t" ++) . show . nextTemp

resetTemp :: CompilerState -> CompilerState
resetTemp state = state { nextTemp = 0 }

addDecl :: ID ->   -- symbol name
           Bool -> -- is temporary
           Type -> -- kind of variable
           CompilerState -> CompilerState
addDecl i tmp typ state = (modifySymbolTable $
    Map.insert i Symbol { isTemp = tmp, symType = typ, symAddr = addr })
    $ state
    where addr = activationRecord . getLocals $ state

findSymbol :: CompilerState -> ID -> Symbol
findSymbol state@CompilerState { blocks = bs, locals = env } i =
    (table $ bs Map.! env) Map.! i

openBlock :: CompilerState -> CompilerState
openBlock state@CompilerState { blocks = bs, nextEnv = env } =
    let new = Scope { table = Map.empty, parent = env }
        env' = env + 1
        in modifyBlocks (Map.insert env' new) state { locals = env', nextEnv = env' }

closeBlock :: CompilerState -> CompilerState
closeBlock state@CompilerState { blocks = bs, locals = env } =
    let p = bs Map.! env
        in state { locals = parent p }

getLocals :: CompilerState -> Scope
getLocals state = blocks state Map.! locals state

modifyBlocks :: (Blocks -> Blocks) -> CompilerState -> CompilerState
modifyBlocks f state = state { blocks = blocks' }
    where blocks' = f $ blocks state

modifySymbolTable :: (SymbolTable -> SymbolTable) -> CompilerState -> CompilerState
modifySymbolTable f state = modifyBlocks g state
    where g bs = Map.insert env b' bs
          b = bs Map.! env
          b' = b { table = f $ table b }
          bs = blocks state
          env = locals state
