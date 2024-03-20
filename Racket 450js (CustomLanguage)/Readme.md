# CS450js Lang (for hw9)

## Overview

CS450js Lang is a simplified s-expression based language designed for homework assignment 9. It features constructs for variables, binding (including recursive binding), conditional expressions, function definitions, and built-in testing forms.

## Syntax

- **450jsExpr (Expressions)**
    - **450jsAtom**
        - Number
        - String
        - SymBool (`realtrue`, `realfalse`)
    - **450jsVariable** (Symbol)
    - **bind** `(list 'bind [Var 450jsExpr] . List<450jsExpr>)`
    - **bind/rec** `(list 'bind/rec [Var 450jsExpr] . List<450jsExpr>)`
    - **iffy** `(list 'iffy 450jsExpr 450jsExpr 450jsExpr)`
    - **fn** `(list 'fn List<Var> 450jsExpr)`
    - **TestExpr** (See Testing)
    - **450jsFnApp** `(cons 450jsExpr List<450jsExpr>)` 

## Abstract Syntax Tree (AST)

- **450jsAST**
     - `(num Number)`
     - `(str String)`
     - `(boo Boolean)`
     - `(vari Symbol)`
     - `(bind Symbol AST AST)`
     - `(bind/rec Symbol AST AST)`
     - `(ite AST AST AST)`
     - `(app AST List<AST>)`
     - `(chk-err AST AST)`
     - `(chk=? AST AST)`
     - `(fn-ast List<Symbol> AST)`

## Results

- **450jsResult**
    - **NoErrorResult**
        - Number (Racket)
        - String (Racket)
        - Boolean (Racket)
        - NaN
        - 450jsFunctionResult
    - **450jsErrorResult**
        - `(undefined-err Symbol)` 
        - `(arity-err 450jsFunctionResult List<450jsResult>)`
        - `(apply-err 450jsResult)`
        - `(circular-err Symbol)`

## Interpreter

- **Conversion Functions**
    - `parse: Expr -> AST` 
    - `res->string : Result -> String`
    - `res->bool : Result -> Boolean`
    - And more...
- **Environment (Env)** - `List<EnvVal>`
    - **EnvVal**
        - `450jsResult`
        - `Box<450jsResult>` (for bind/rec)
- **Key Functions**
    - `run: 450jsAST Env -> 450jsResult`
    - `lookup : Var Env -> Result`
    - `450apply : 450jsResult List<450jsAST> Env -> 450jsResult`

## Testing

- **chk=?** `(list 'chk=? 450jsExpr 450jsExpr)` (Equivalent to Racket's `check-equal?`)
- **chkerr**  `(list 'chkerr 450jsExpr 450jsExpr)` (For testing error-producing code)

## Examples

(Provide extensive code examples demonstrating language features, testing, and error handling)

# myApp.rkt

This file contains several recursive functions written in the CS450JS language. Here's a brief overview of each function:

- **fac**: This is a recursive function that calculates the factorial of a number.
- **filt**: This function filters a list based on a predicate function.
- **qsort**: This function implements the quicksort algorithm.
- **gcd**: This function calculates the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.

For more details, please refer to the source code of `myApp.rkt`.

Please let me know if you'd like any sections expanded or specific usage scenarios illustrated!