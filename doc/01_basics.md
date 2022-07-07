# Basics

To start an interactive REPL session, use `run` with no arguments. The REPL can
be exited with `:quit`.

To run a Cooma program, use `run -r` followed by the path to the source file
and any program arguments.

Use `run --help` for more options.

## Declarations

Integer and string literals:

```
42
> res0 : Int = 42

"Hello, world!"
> res1 : String = "Hello, world!"
```

Value declarations:

```
val x = 100
> x : Int = 100

val y = x + 1
> y : Int = 101
```

Functions can be defined using the `def` keyword:

```
def double(x : Int) Int = x * 2
> addOne : (x : Int) Int = <function>

double(7)
> res2 : Int = 8
```

Functions can be partially applied:

```
def add(a : Int, b : Int) Int = a + b
> add : (a : Int, b : Int) Int = <function>

val addSeven = add(7)
> addSeven : (b : Int) Int = <function>

addSeven(3)
res13 : Int = 10
```

Comments are preceded by '`//`'.

## Expressions

Anonymous function literals:

```
val triple = fun (x : Int) => x * 3
> triple : (x : Int) Int = <function>

triple(10)
> res4 : Int = 30
```

Braces are used for block expressions and grouping:

```
val y = {
    val x = 2
    x * x
}
> y : Int = 4

{ 1 + 2 } * 3
> res5 : Int = 9
```

Conditionals:

```
if 1 < 2 then "asdf" else "zxcv"
> res6 : String = "asdf"
```

Conditional expressions are just a syntactic sugar for a special case of the
match expression, which we will see later. 

## Types

Cooma is a strongly typed language. Type annotations are required for the
arguments and return of a `def`, and for the arguments of an anonymous function
literal. Elsewhere, types can generally be inferred and thus type annotations
are optional.

```
val s : String = 4
> 1:18:error: expected String, got 4 of type Int
> val s : String = 4
>                  ^
```

Type aliases can be declared using the `type` keyword:

```
type A = Int
> A : Type = Int

val a : A = 16
> a : A = 16
```

## Programs

A Cooma program is just an expression whose output is the result of evaluating
that expression.

This program is a program that outputs `Hello, world!`:

```
"Hello, world!"
```

To accept command-line arguments, the top-level expression should be a
function whose arguments correspond to command-line arguments.

This program behaves like the `cat` command, outputting its input:

```
fun (s : String) s
```

Sometimes it is useful to have type declarations preceding the program
function. If a Cooma program consists of a block expression whose final
expression is a function literal, that function literal will be treated as the
program function.

```
{
  type Text = String
  fun (text : Text) text
}
```

(This is a fairly trivial example, later we will see examples where this is
necessary to prevent lengthy argument type annotations and code duplication.)

## Prelude

A number of useful definitions can be found in the prelude, which is imported
into every Cooma program. These include primitive operations, definitions for
the capability types, primitive operations, and common type constructors.
