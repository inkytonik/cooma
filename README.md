# Cooma

The Cooma project at Macquarie University is investigating secure programming language design based on fine-grained object capabilities.

[![License: MPL v2.0](https://img.shields.io/badge/License-MPL%20v2-blue.svg)](http://mozilla.org/MPL/2.0/)

## Participants

* Anthony Sloane (Anthony.Sloane@mq.edu.au)
* Franck Cassez
* Diego Ocampo Herrera
* Scott Buckley

## Sponsors

![Oracle](https://plvmq.bitbucket.io/SAPLING/images/Oracle%20Red%20Badge.png)

## Features

* Functional core compiled to a continuation-passing intermediate representation (["Compiling with continuations, continued", Kennedy, ICFP 2007](https://doi.org/10.1145/1291151.1291179))

* Row-based data types capable of encoding types with variants and object-oriented style extension (["Abstracting extensible data types: or, rows by any other name", Morris and McKinna, POPL 2019](https://doi.org/10.1145/3290325))

* Fine-grained object capability-based effects (["A Study of Capability-Based Effect Systems", Liu, EPFL, 2016](https://github.com/liufengyun/stoic))

* Resource capabilities checked and provided by runtime system

* Implicit argument resolution to avoid passing many capability parameters (["COCHIS: Stable and coherent implicits", Schrivjers, Oliveira, Wadler and Mantirosian, JFP, 2019](http://dx.doi.org/10.1017/s0956796818000242))

## Status

Specification and reference implementation is under way.

* Functional core (tail call optimisation, but no polymorphism)
* Row-based data types (literals, selection, concatenation, no variants)
* Object capabilities via rows
* Runtime-provided resource capabilities (Console and Reader only)
* Implicit argument resolution (not started)

### Related projects

* [Kiama language processing library](https://bitbucket.org/inkytonik/kiama)
* [sbt-rats parser generator](https://bitbucket.org/inkytonik/sbt-rats)

## Implementation

### Prerequisites

* [Scala Build Tool - sbt](https://www.scala-sbt.org)

### Running

* clone this project
* go to the cloned directory
* run `sbt`
* at the sbt prompt, run the Cooma implementation

E.g., for the program `src/test/resources/multiArgCall.cooma` which is a simple multiple argument function call:

```ml
{fun (x : Int, y : String) => x} (10, "hello")
```

we get the following using the `-r` option to print the program result:

```ml
$ sbt
[info] Loading settings for project global-plugins from metals.sbt ...
... more loading messages ...
cooma 0.1.0 2.12.8> run -r src/test/resources/multiArgCall.cooma`
[info] Running (fork) org.bitbucket.inkytonik.cooma.Main -r src/test/resources/multiArgCall.cooma
[info] 10
```

Use `--help` to see all of the options for printing the source AST, IR and IR AST. E.g., use `-i` to print the IR AST:

```ml
cooma 0.1.0 2.12.8> run -i -r src/test/resources/multiArgCall.cooma
[info] Running (fork) org.bitbucket.inkytonik.cooma.Main -i -r src/test/resources/multiArgCall.cooma
[info] letv f3 = fun k4 x => letv f5 = fun j6 y => j6 x in
[info]     k4 f5 in
[info]     letv x7 = 10 in
[info]         letc k1 x2 = letv x8 = "hello" in
[info]             x2 halt x8 in
[info]             f3 k1 x7
[info] 10
```

### Testing

Use the sbt command `test` to run the automated tests.

## Examples

NOTE: sbt `[info]` markers have been removed to simplify the output.

### Row argument and field reference

```ml
{fun (r : {x : Int, y : Int, z : String}) => r.x} ({x = 20, y = 10, z = "Hi"})

> run -r src/test/resources/rowArg.cooma
20
```

### Row concatenation

```ml
{
    val r = {x = 10, y = 20}
    val s = {a = "Hi"}
    {r & s}.x
}

> run -r src/test/resources/rowConcat.cooma
10
```

### Blocks (values and function definitions)

```ml
{
    val x = 10
    val y = 20
    y
}

> run -r src/test/resources/blockVal.cooma
20
```

```ml
{
    def f (x : Int) = x
    def g (y : Int) = f(y)
    g(10)
}

> run -r src/test/resources/blockDef.cooma
10
```

### String command-line arguments

```ml
fun (s : String) => s

> run -r src/test/resources/stringCmdArg.cooma hello
hello

fun (s : String, t : String) => t

> run -r src/test/resources/multiStringCmdArg.cooma hello there
there
```

### Console capability

```ml
fun (c : Console) => c.write("Hello world!\n")

> run -r src/test/resources/consoleCmdArg.cooma /dev/tty
Hello world!
{}
```

`{}` is the unit value returned by `c.write`.

If the specified file name is not writeable, the runtime system causes the execution to fail.

```ml
> run src/test/resources/consoleCmdArg.cooma /does/not/exist
cooma: Console capability unavailable: can't write /does/not/exist
```

### Console and Reader capabilities

```ml
fun (c : Console, r : Reader) => c.write(r.read({}))

> run -r src/test/resources/consoleReaderCmdArg.cooma /dev/tty src/test/resources/multiArgCall.cooma
(fun (x : Int, y : String) => x) (10, "hello")
{}
```

A Reader capability is only provided if the designated file can be read.

```ml

> run src/test/resources/consoleReaderCmdArg.cooma /dev/tty /does/not/exist
cooma: Reader capability unavailable: can't read /does/not/exist
```
