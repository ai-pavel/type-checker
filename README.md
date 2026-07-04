# type-checker

[![CI](https://github.com/ai-pavel/type-checker/actions/workflows/ci.yml/badge.svg)](https://github.com/ai-pavel/type-checker/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/ai-pavel/type-checker/branch/main/graph/badge.svg)](https://codecov.io/gh/ai-pavel/type-checker)

A Hindley-Milner type inference engine implemented in Haskell.

## Features

- Lambda calculus AST with let-bindings, if-then-else, and literals (int, bool, string)
- Algebraic data types (sum and product types) with pattern matching
- Algorithm W for type inference with unification, substitution, and let-polymorphism
- Megaparsec-based parser
- Interactive REPL

## Building

```
stack build
```

## Running the REPL

```
stack run
```

## Examples

```
> \x -> x
a -> a

> let id = \x -> x in id 42
Int

> \f -> \x -> f (f x)
(a -> a) -> a -> a

> if True then 1 else 2
Int
```

## Testing

```
stack test
```

## Project Structure

- `src/Infer/Ast.hs` - Abstract syntax tree definitions
- `src/Infer/Types.hs` - Type representation, substitution, schemes
- `src/Infer/Unify.hs` - Unification algorithm
- `src/Infer/Infer.hs` - Algorithm W type inference
- `src/Infer/Parser.hs` - Megaparsec parser
- `app/Main.hs` - REPL entry point
- `test/Spec.hs` - Hspec tests
