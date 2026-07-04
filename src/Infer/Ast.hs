module Infer.Ast
  ( Expr(..)
  , Pattern(..)
  , Lit(..)
  , Name
  , CaseBranch
  ) where

-- | Variable names
type Name = String

-- | A branch in a case/match expression: (pattern, body)
type CaseBranch = (Pattern, Expr)

-- | Literal values
data Lit
  = LInt Integer
  | LBool Bool
  | LString String
  deriving (Show, Eq)

-- | Patterns for pattern matching
data Pattern
  = PVar Name                   -- ^ Variable pattern (matches anything)
  | PLit Lit                    -- ^ Literal pattern
  | PCon Name [Pattern]         -- ^ Constructor pattern (e.g., Just x, Pair a b)
  | PWild                       -- ^ Wildcard pattern _
  deriving (Show, Eq)

-- | Core expression AST
data Expr
  = EVar Name                   -- ^ Variable reference
  | ELit Lit                    -- ^ Literal value
  | EApp Expr Expr              -- ^ Function application
  | ELam Name Expr              -- ^ Lambda abstraction
  | ELet Name Expr Expr         -- ^ Let binding (with let-polymorphism)
  | EIf Expr Expr Expr          -- ^ If-then-else
  | ECon Name                   -- ^ Data constructor
  | ETuple [Expr]               -- ^ Tuple / product type
  | ECase Expr [CaseBranch]     -- ^ Pattern matching (case expr of ...)
  deriving (Show, Eq)
