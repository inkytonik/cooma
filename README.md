# Cooma

The Cooma project at Macquarie University is investigating secure programming language design based on fine-grained object capabilities.

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

* Functional core (no polymorphism)
* Row-based data types (simple creation and field ref, no extension)
* Object capabilities via rows
* Runtime-provided resource capabilities (Console and Reader only)
* Implicit argument resolution (not started)

### Related projects

* [Kiama language processing library](https://bitbucket.org/inkytonik/kiama)
* [sbt-rats parser generator](https://bitbucket.org/inkytonik/sbt-rats)

## Implementation

### Prerequisites

* [Scala Build Tool](https://www.scala-sbt.org)

### Running

Clone this project, go to the cloned directory, run `sbt`, then at the sbt prompt, run the Cooma implementation.

E.g., for the program `tests/test1.cooma` which is a simple multiple argument function call:

```ml
(fun (x : Int, y : String) => x) (10, "hello")
```

we get the following using the `-r` option to print the program result:

```ml
$ sbt
[info] Loading settings for project global-plugins from metals.sbt ...
... more loading messages ...
cooma 0.1.0 2.12.8> run -r tests/test1.cooma
[info] Running (fork) org.bitbucket.inkytonik.cooma.Main -r tests/test1.cooma
[info] 10
```

Use `--help` to see all of the options for printing the source AST, IR and IR AST. E.g., use `-i` to print the IR AST:

```ml
cooma 0.1.0 2.12.8> run -i -r tests/test1.cooma
[info] Running (fork) org.bitbucket.inkytonik.cooma.Main -i -r tests/test1.cooma
[info] letv f5 = fun k6 x => letv f7 = fun k8 y => k8 x in
[info]     k6 f7 in
[info]     letv x9 = 10 in
[info]         letc k3 x4 = letv x10 = "hello" in
[info]             letc k1 x2 = halt x2 in
[info]                 x4 k1 x10 in
[info]             f5 k3 x9
[info] 10
```

## More Examples

NOTE: sbt `[info]` markers have been removed to simplify the output.

### Row argument and field reference

```ml
(fun (r : {x : Int}) => r.x) ({x = 20})

> run -i -r tests/test2.cooma
letv f3 = fun k4 r => letv x5 = r.x in
    k4 x5 in
    letv x7 = 20 in
        letv x6 = {x = x7} in
            letc k1 x2 = halt x2 in
                f3 k1 x6
20
```

### String command-line arguments

```ml
fun (s : String) => s

> run -i -r tests/test3.cooma hello
letv s = arg 0 in
    halt s
hello

fun (s : String, t : String) => t

> run -i -r tests/test4.cooma hello there
letv s = arg 0 in
    letv t = arg 1 in
       halt t
there
```

### Console capability

```ml
fun (c : Console) => c.write("Hello world!")

> run -i -r tests/test5.cooma /dev/tty
letv x1 = arg 0 in
    letv c = cap Console x1 in
        letv x4 = c.write in
            letv x5 = "Hello world!" in
                letc k2 x3 = halt x3 in
                    x4 k2 x5
"Hello world!"{}```

`{}` is the unit value returned by `c.write`.

NOTE: Cooma doesn't yet support escape sequences in strings, so there is no newline, and the quotes will eventually be removed from the string.

If the specified file name is not writeable, the runtime system causes the execution to fail.

```ml
> run tests/test5.cooma /does/not/exist
cooma: Console capability unavailable: can't write /does/not/exist
```

### Console and Reader capabilities

```ml
fun (c : Console, r : Reader) => c.write(r.read())

> run -i -r tests/test6.cooma /dev/tty tests/test1.cooma
letv x1 = arg 0 in
    letv c = cap Console x1 in
        letv x2 = arg 1 in
            letv r = cap Reader x2 in
                letv x5 = c.write in
                    letv x9 = r.read in
                        letv z8 = {} in
                            letc k6 x7 = letc k3 x4 = halt x4 in
                                x5 k3 x7 in
                                x9 k6 z8
(fun (x : Int, y : String) => x) (10, "hello") {}```

A Reader capability is only provided if the designated file can be read.

```ml

$ cat >cantread.txt
secret
$ chmod 600 cantread.txt
$ sudo chown root cantread.txt

> run tests/test6.cooma /dev/tty cantread.txt
cooma: Reader capability unavailable: can't read cantread.txt
```
