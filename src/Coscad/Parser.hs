-- | Parser for .coscad files: glyph shapes, word shapes, boolean
-- operators, attachment ops, and |> pipelines with multi-line
-- continuation. Also the variable-resolution pass.
module Coscad.Parser
  ( Ctx (..)
  , Def (..)
  , Parser
  , SynMode (..)
  , VarTable
  , anchorVec
  , checkedPragma
  , double
  , expressionString
  , frameRef
  , identifier
  , lexeme
  , num
  , numericPhase
  , parseExpression
  , parseProgram
  , parseProgramNamed
  , parseProgramFull
  , prettyBundle
  , reservedNames
  , resolveVariables
  , sc
  , symbol
  , trim
  , variableDefinition
  ) where

import Coscad.Parser.Core (Ctx (..), Parser, SynMode (..), VarTable, anchorVec, double, frameRef, identifier, lexeme, num, reservedNames, sc, symbol, trim)
import Coscad.Parser.Program (Def (..), checkedPragma, expressionString, numericPhase, parseExpression, parseProgram, parseProgramNamed, parseProgramFull, prettyBundle, resolveVariables, variableDefinition)

