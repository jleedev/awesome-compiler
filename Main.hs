-- |
-- @
-- EECS 337
-- Homework 6
-- Josh Lee@
--
-- Rather than rewrite the compiler from scratch using Flex and Bison
-- again, I chose to use this opportunity to explore the functional
-- programming language Haskell [1]. Being statically and strongly
-- typed, Haskell provides the advantage of catching most errors at
-- compile time: if an error occurs during the program execution, it is
-- a logical error.
--
-- This parser is written in Haskell using the Parsec [2] parser
-- library. Parsec, being a combinator library, takes a different
-- approach to parsing than Bison. Code is not generated from a grammar
-- file; rather, the productions are actual functions that consume input
-- and produce objects from input. The downside to this approach is that
-- the grammar must be written more carefully -- left recursion, for
-- example, is not allowed since it would never consume any input.
--
-- It is easier, however, to write more complex parsers. Parsec provides
-- a @squares@ parser, for example, that allows any given parser to
-- parse the same thing wrapped in square brackets. Another parser,
-- @many@, applies a given parser until it fails and returns the results
-- in a list. Putting these together, @many (squares natural)@ is used
-- in the definition of 'parseDecl' to parse a sequence of natural
-- numbers in square brackets, exactly what is needed for our variable
-- declarations.
--
-- Functional programming in Haskell provides a challenge. It is
-- impossible to easily mix I\/O side effects with pure code, and the
-- code will not compile until every type error is corrected. While this
-- has obvious advantages over some of the unsafe tricks we can do with
-- C, it requires a much more thoughtful approach to organizing the
-- program.
--
-- As it stands, this code does not generate much three address code.
-- It does, however, properly parse declarations. When a declaration is
-- read, it is added to the symbol table, and its address relative to
-- the current scope is computed. When the sample input
--
-- @ { float[5][7] i; int j; int[3] k; { int i; float[3][3] j; } }@
-- 
-- is entered into the program, it outputs the following:
--
-- @
-- \> Env 1
-- \> Env 2
-- \> Closing block Scope {table = fromList [(\"i\",int at 0),(\"j\",float[3][3] at 4)], parent = 1}
-- \> Closing block Scope {table = fromList [(\"i\",float[5][7] at 0),(\"j\",int at 280),(\"k\",int[3] at 284)], parent = 0}@
--
-- The variables @i@ and @j@ in the inner scope, followed by @i@, @j@
-- and @k@ in the outer scope, have been given the correct addresses.
--
-- The included binary was compiled with GHC 6.6.1 on a 32-bit Linux
-- machine. This documentation was generated automatically from the
-- source code using Haddock [3]. Parsec and Haddock are both
-- distributed with GHC [4].
--
-- (1) <http://haskell.org/>
--
-- (2) <http://legacy.cs.uu.nl/daan/parsec.html>
--
-- (3) <http://haskell.org/haddock/>
--
-- (4) <http://haskell.org/ghc/>

module Main (main) where

import Parser
import IO

-- | Read a program from standard input, printing debugging information
-- about its symbols.
main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    c <- getContents
    print (test c)
