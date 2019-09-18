# Cooma

The Cooma project at Macquarie University is investigating secure programming language design based on fine-grained object capabilities.

[![License: MPL v2.0](https://img.shields.io/badge/License-MPL%20v2-blue.svg)](http://mozilla.org/MPL/2.0/)

## Participants

* Anthony Sloane (Anthony.Sloane@mq.edu.au)
* Diego Ocampo Herrera
* Cameron Pappas
* Scott Buckley

## Sponsors

![Oracle](https://plvmq.bitbucket.io/SAPLING/images/Oracle%20Red%20Badge.png)

## Features

* Functional core compiled to a continuation-passing intermediate representation (["Compiling with continuations, continued", Kennedy, ICFP 2007](https://doi.org/10.1145/1291151.1291179))

* Row-based data types capable of encoding record types with variants and object-oriented style extension (["Abstracting extensible data types: or, rows by any other name", Morris and McKinna, POPL 2019](https://doi.org/10.1145/3290325))

* Fine-grained object capability-based effects (["A Study of Capability-Based Effect Systems", Liu, EPFL, 2016](https://github.com/liufengyun/stoic))

* Resource capabilities checked and provided by runtime system

* Implicit argument resolution to avoid passing many capability parameters (["COCHIS: Stable and coherent implicits", Schrivjers, Oliveira, Wadler and Mantirosian, JFP, 2019](http://dx.doi.org/10.1017/s0956796818000242))

## Status

Specification and reference implementation is under way.

* Functional core with tail call optimisation
* Row-based record data types (literals, selection, concatenation, no variants)
* Object capabilities via records
* Runtime-provided resource capabilities (I/O operations only)
* Implicit argument resolution (not started)
* Single shared frontend (parsing, semantic analysis, compilation to continuation-based IR)
* Two IR backends (reference and Truffle/GraalVM)
* File-based execution and read-eval-print loop (REPL)

### Related projects

* [Kiama language processing library](https://bitbucket.org/inkytonik/kiama)
* [sbt-rats parser generator](https://bitbucket.org/inkytonik/sbt-rats)

## Implementation

### Prerequisites

* [Scala Build Tool - sbt](https://www.scala-sbt.org)

### Running

* clone this project
* go to the cloned directory
* run `sbt` to get cooma project prompt

### Running interactively (REPL mode)

```ml
root 0.1.0 2.12.8> run
[info] ... sbt messages ...
roor 0.1.0 REPL - Reference backend

Enter definitions or expressions (:help for commands)

cooma> :help

exp                          evaluate exp, print value
val x = exp                  add new value definition
def f(x : Int) : Int = exp   add new function definition
:help                        print this message
:lines                       enter multiple separate input lines until :end
:paste                       enter single multi-line input until :end
:quit                        quit the REPL (also Control-D)

cooma> 10
res0 = 10

cooma> res0
res1 : Int = 10

cooma> val x = 20
x : Int = 20

cooma> x
res2 : Int = 20

cooma> def f(x : Int) = x
f : (Int) => Int = <function>

cooma> f(20)
res3 : Int = 20

cooma> :lines
val a = 1
val b = 2
:end
a : Int = 1
b : Int = 2

cooma> a
res4 : Int = 1

cooma> :paste
{
   val c = 1
   val d = 2
   c
}
:end
res5 : Int = 1
```

### Running on files (compiler mode)

E.g., for the program `reference/src/test/resources/basic/multiArgCall.cooma` which is a simple multiple argument function call:

```ml
{fun (x : Int, y : String) = x} (10, "hello")
```

we get the following using the `-r` option to print the program result:

```ml
cooma 0.1.0 2.12.8> run -r reference/src/test/resources/basic/multiArgCall.cooma`
[info] ... sbt messages ...
10
```

Use `run --help` to see all of the options for printing the source AST, IR and IR AST.
E.g., use `-i` to print the IR AST:

```ml
cooma 0.1.0 2.12.8> run -i -r reference/src/test/resources/basic/multiArgCall.cooma
[info] Running (fork) org.bitbucket.inkytonik.cooma.Main -i -r reference/src/test/resources/basic/multiArgCall.cooma
letv f5 = fun k6 x = letv f7 = fun j8 y = j8 x
                     k6 f7
letv x9 = 10
letc k3 x4 = letv x10 = "hello"
             letc k1 x2 = $halt x2
             x4 k1 x10
f5 k3 x9
10
```

### Testing

Use the sbt command `test` to run the automated tests.

## Examples

NOTE: sbt `[info]` markers have been removed to simplify the output.

### Blocks (values and function definitions)

```ml
{
    val x = 10
    val y = 20
    y
}

> run -r reference/src/test/resources/basic/blockVal.cooma
20
```

```ml
{
    def f (x : Int) : Int = x
    def g (y : Int) : Int = f(y)
    g(10)
}

> run -r reference/src/test/resources/basic/blockDef.cooma
10
```

### Record argument and field reference

```ml
{fun (r : {x : Int, y : Int, z : String}) = r.x} ({x = 20, y = 10, z = "Hi"})

> run -r reference/src/test/resources/basic/recordArg.cooma
20
```

### Record concatenation

```ml
{
    val r = {x = 10, y = 20}
    val s = {a = "Hi"}
    {r & s}.x
}

> run -r reference/src/test/resources/basic/recordConcat.cooma
10
```

### String command-line arguments

```ml
fun (s : String) = s

> run -r reference/src/test/resources/capability/stringCmdArg.cooma hello
"hello"

fun (s : String, t : String) = t

> run -r reference/src/test/resources/capability/multiStringCmdArg.cooma hello there
"there"
```

### Writer capability

Capability arguments at the top-level are automatically linked with the command-line arguments and checked.
E.g., a Writer capability allows the program to write to the named file or device.
(The name "-" means standard output, or input for readers.)

```ml
fun (w : Writer) = w.write("Hello world!\n")

> run -r reference/src/test/resources/capability/writerCmdArg.cooma /dev/tty
Hello world!
{}
```

`{}` is the unit value returned by `w.write`.

If the specified file name is not writeable, the runtime system causes the execution to fail.

```ml
> run reference/src/test/resources/capability/writerCmdArg.cooma /does/not/exist
cooma: Writer capability unavailable: can't write /does/not/exist
```

### Writer and Reader capabilities

```ml
fun (w : Writer, r : Reader) = w.write(r.read())

> run -r reference/src/test/resources/capability/writerAndReaderCmdArg.cooma /dev/tty reference/src/test/resources/basic/multiArgCall.cooma
(fun (x : Int, y : String) = x) (10, "hello")
{}
```

A Reader capability is only provided if the designated file can be read.

```ml

> run reference/src/test/resources/capability/consoleReaderCmdArg.cooma /dev/tty /does/not/exist
cooma: Reader capability unavailable: can't read /does/not/exist
```

The built-in ReaderWriter capability combines Reader and Writer.
