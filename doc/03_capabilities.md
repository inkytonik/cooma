# Capabilities

If a Cooma program is a function, the arguments to the function are interpreted
as command-line arguments:

```
fun (x : String) Strings.length(x)
```

For instance, if this program is run with `asdf` as its argument, the program
will evaluate to `4`.

Command-line arguments may also correspond to _capabilities_, such as the
ability to read or write to a particular file or access a particular server.

## File IO capabilities

A program that evaluates to the text of a file specified by the user can be
written as follows:

```
fun (file : Reader) file.read()
```

Here, the command-line argument is ascribed the capability type `Reader`. When
the program is executed, the command-line argument is interpreted as the path
to a file that the program is allowed to read. There is a pre-runtime check to
ensure that the user executing the program has permissions to read the file.
The `read` function reads the contents of the file and returns the text as a
string.

While `Reader` as an command-line argument type directs the compiler to treat
it specially, from the programmer's perspective, `Reader` is just a record
type:

```
type Reader = { read : () Result(String) }
```

Within Cooma, there is no other way for a program to access files outside of
command-line arguments. It is the responsibility of the programmer to specify
what files and permissions are needed to run the program, and the
responsibility of the user to provide and specify them.

If we want the program to be able to write to a file, we can use the `Writer`
capability type instead:

```
fun (file : Writer) file.write("Hello, world!")
```

We can also use record concatenation to combine capabilities. Let's say we want
to write a program that reads the contents of a file, overwrites it, and then
returns its previous contents. Clearly, the program needs both read and write
access to the file. We can write the program like so:

```
fun (file : Reader & Writer) {
    val contents = file.read()
    val _ = file.write("Hello, world!")
    contents
}
```

By ascribing `file` the type `Reader & Writer`, the program is granted both
capabilities to the file. Note how this also makes sense from the perspective
that capability types are "just record types". Expanded, `Reader & Writer` is
just `{ read : Result(String), write : Result(Unit) }`, which is what the
programmer expects from `file`.

Cooma also offers the `FolderReader` and `FolderWriter` capability types, which
treat the command-line argument as a path to a directory and allow the program
to read or write (respectively) to any file contained (recursively) in that
directory.

There is also a `Runner` capability type to execute a file as a program.

## HTTP capabilities

Cooma offers capability types that allow sending HTTP requests:

```
fun (client : HttpGet) client.get("")
```

Running this program will treat the command-line argument as a URL and return
the content at that URL as a string.

The string argument passed to `get` can be used to append a path or query
parameters to the command-line argument.

There are also `HttpPost`, `HttpPut`, and `HttpDelete` capabilities, which
provide functions that will use the corresponding HTTP method in the request.
Like file IO capabilities, these capabilities can be combined using record type
concatenation.

## Database capability

The `Database` capability provides access to a database. (Currently only SQLite
is supported.) `Database` is a type constructor whose argument is a record type
representing the database schema. A user running the program will provide the
path to the database as the command-line argument, and the Cooma runtime will
verify that the schema provided by the programmer is compatible with the actual
database.

Suppose we have a database with the following SQL schema:

```
CREATE TABLE student (
    id INTEGER PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    wam INTEGER
);

CREATE TABLE subject (
    id INTEGER PRIMARY KEY NOT NULL,
    code INTEGER NOT NULL,
    name TEXT NOT NULL
);
```

A program accepting this database might look like this:

```
fun (
  db : Database({
    student : Table({
      id : Int,
      name : String,
      wam : Option(Int)
    }),
    subject : Table({
      id : Int,
      code : Int,
      name : String
    })
  })
) …
```

Currently, it is required that each table have an integer `id` column as the
(sole) primary key.

The tables of `db` can be accessed as `db.student` and `db.subject`.

Each table has the `all`, `getById`, `insert`, `update`, and `delete` methods.
