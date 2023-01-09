# Cooma

The Cooma project is investigating secure programming language design based on fine-grained object capabilities.

[![License: MPL v2.0](https://img.shields.io/badge/License-MPL%20v2-blue.svg)](http://mozilla.org/MPL/2.0/)
![Test](https://github.com/inkytonik/cooma/workflows/Test/badge.svg)

## Getting started

### Prerequisites

* [Scala Build Tool - sbt](https://www.scala-sbt.org)
* [Cram shell testing tool](https://bitheap.org/cram/)

### Running

* Clone this project.
* Go to the cloned directory.
* Run `sbt`.

### Running interactively (REPL mode)

To enter REPL mode, type `run`.

```ml
root 0.1.0 2.13.9> run
[info] ... sbt messages ...
root 0.1.0 REPL - Reference backend

Enter definitions or expressions (:help for commands)

cooma> :help

exp                         evaluate exp, print value
val x = exp                 add new value definition
def f (x : Int) Int = exp   add new function definition
:help                       print this message
:lines                      enter multiple separate input lines until :end
:paste                      enter single multi-line input until :end
:quit                       quit the REPL (also Control-D)

cooma> val x = 20
x : Int = 20

cooma> x
res0 : Int = 20

cooma> def f (x : Int) Int = x
f : (Int) Int = <function>

cooma> f(x)
res1 : Int = 20
```

### Running on files (compiler mode)

E.g., for the program `src/test/resources/basic/multiArgCall.cooma` which is a simple multiple argument function call:

```ml
{fun (x : Int, y : String) x} (10, "hello")
```

we get the following using the `-r` option to print the program result:

```ml
cooma 0.1.0 2.13.9> run -r src/test/resources/basic/multiArgCall.cooma
[info] ... sbt messages ...
10
```

Use `run --help` to see all of the options for printing the source AST.

### Testing

Use the shell command `./test` to run the automated tests.

## Examples

See the [tutorials](doc) for further examples.

NOTE: sbt `[info]` markers have been removed to simplify the output.

### Blocks (values and function definitions)

```ml
{
    val x = 10
    val y = 20
    y
}

> run -r src/test/resources/basic/blockVal.cooma
20
```

```ml
{
    def f (x : Int) Int = x
    def g (y : Int) Int = f(y)
    g(10)
}

> run -r src/test/resources/basic/blockDef.cooma
10
```

### Record argument and field reference

```ml
{fun (r : {x : Int, y : Int, z : String}) r.x} ({x = 20, y = 10, z = "Hi"})

> run -r src/test/resources/basic/recordArg.cooma
20
```

### Record concatenation

```ml
{
    val r = {x = 10, y = 20}
    val s = {a = "Hi"}
    {r & s}.x
}

> run -r src/test/resources/basic/recordConcat.cooma
10
```

Read more about records [here](doc/02_composite-types.md).

### String command-line arguments

```ml
fun (s : String) s

> run -r src/test/resources/capability/stringCmdArg.cooma hello
"hello"

fun (s : String, t : String) t

> run -r src/test/resources/capability/multiStringCmdArg.cooma hello there
"there"
```

### Capabilities

Capability arguments at the top-level are automatically linked with the command-line arguments and checked.
E.g., a Writer capability allows the program to write to the named file or device.
(The name "-" means standard output, or input for readers.)

```ml
fun (w : Writer) w.write("Hello world!\n")

> run -r src/test/resources/capability/writerCmdArg.cooma /dev/tty
Hello world!
{}
```

`{}` is the unit value returned by `w.write`.

If the specified file name is not writeable, the runtime system causes the execution to fail.

```ml
> run src/test/resources/capability/writerCmdArg.cooma /does/not/exist
cooma: Writer capability unavailable: can't write /does/not/exist
```

A file capability is only provided if the designated file can be read.

```ml

> run src/test/resources/capability/readerCmdArg.cooma /does/not/exist
cooma: Reader capability unavailable: can't read /does/not/exist
```

The Reader and Writer capabiities can be combined into a single argument using type concatenation.

```ml
fun (rw : Reader & Writer) {
    val result = rw.read()
    val _ = rw.write("Hello, world!\n")
    result
}
```

Read more about capabilities [here](doc/03_capabilities.md).

## Participants

* Anthony Sloane (inkytonik@gmail.com)

## Previous Participants

* Nicholas Weston
* Diego Ocampo Herrera
* Cameron Pappas
* Scott Buckley

## Sponsors

![Oracle](https://plvmq.bitbucket.io/SAPLING/images/Oracle%20Red%20Badge.png)

## Related projects

* [Kiama language processing library](https://bitbucket.org/inkytonik/kiama)
* [sbt-rats parser generator](https://bitbucket.org/inkytonik/sbt-rats)
