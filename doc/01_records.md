# Records

A _record_ is a collection of named fields:

```
val student = { name = "John Smith", wam = 90 }
> student : { name : String, wam : Int } = { name = "John Smith", wam = 90 }

student.name
> res0 : String = "John Smith"
```

Records are _structurally_ typed:

```
{
    def addAB(x : { a : Int, b : Int }) Int = x.a + x.b
    val x = { c = "qwerty", b = 100, a = 42 }
    addAB(x)  // okay!
}
> 142
```

Records can be _concatenated_ using the '`&`' operator:

```
val x = { a = 1, b = 2 }
val y = { c = "asdf" }
x & y
res1 : { a : Int, b : Int, c : String } = { a = 1, b = 2, c = "asdf" }
```

The operands of record concatenation must not have overlapping fields:

```
val x = { a = 1, b = 2 }
val y = { b = 3, c = "asdf" }
> 1:1:error: record concatenation has overlapping field(s) b
> x & y
> ^
```

Record _types_ can also be concatenated:

```
type T = { a : Int, b : Int }
type U = { c : String }
T & U
> res2 : Type = { a : Int, b : Int, c : String }
```
