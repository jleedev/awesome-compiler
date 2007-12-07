-- |
-- *EECS 337
--
-- *Homework 6
-- 
-- *Josh Lee
--
-- Rather than rewrite the compiler from scratch using Flex and
-- Bison again, I chose to use this opportunity to explore the
-- functional programming language Haskell [1]. Being staticly and
-- strongly typed, Haskell provides the advantage of catching most
-- errors at compile time: if an error occurs during the program
-- execution, it is usually a logical error.
--
-- This parser is written in Haskell using the Parsec [2] parser
-- library. Parsec, being a combinator library, takes a different
-- approach to parsing than Bison. Code is not generated from a grammar
-- file; rather, the productions are actual functions that consume input
-- and produce objects from input. The downside to this approach is that
-- the grammar must be written more carefully -- left recursion, for
-- example, is not allowed since it would never consume any input!
--
-- It is easier, however, to write more complex parsers. Parsec provides
-- a @squares@ parser, for example, that allows any given parser to
-- parse the same thing wrapped in square brackets. Another parser,
-- @many@, applies a given parser until it failes and returns the
-- results in a list. Putting these together, @many (squares natural)@
-- is used in the definition of 'parseDecl' to parse a sequence of
-- natural numbers in square brackets, exactly what is needed for our
-- variable declarations.
--
-- Functional programming provides a challenge.
--
-- As it stands, this code does not generate much three address code. It
-- does, however, properly parse declarations. When a declaration is
-- read, it is added to the symbol table, and its address relative to
-- the current scope is computed. When the sample input
--
-- @
-- { float[5][7] i; int j; int[3] k; { int i; float[3][3] j; } }
-- @
-- 
-- is entered into the program, it outputs the following:
--
-- @
-- \> Env 1
-- \> Env 2
-- \> Closing block Scope {table = fromList [("i",int at 0),("j",float[3][3] at 4)], parent = 1}
-- \> Closing block Scope {table = fromList [("i",float[5][7] at 0),("j",int at 280),("k",int[3] at 284)], parent = 0}
-- @
--
-- The variables @i@ and @j@ in the inner scope, followed by @i@, @j@
-- and @k@ in the inner scope, have been given the correct addresses.
--
-- This documentation was generated automatically from the source code
-- using Haddock.
--
-- (1) <http://haskell.org/>
--
-- (2) <http://legacy.cs.uu.nl/daan/parsec.html>
--
-- (3) <http://haskell.org/haddock/>

module Main where

import Parser
import IO

main = do
    hSetBuffering stdin LineBuffering
    c <- getContents
    print (test c)
