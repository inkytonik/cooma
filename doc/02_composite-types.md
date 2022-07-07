# Composite types

## Records

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
x & y
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

## Variants

A _variant_ is a tagged union. A variant type specifies one or more fields,
however a value of that type will only contain _one_ of those fields.

```
type IntOrString = << IntValue : Int, StringValue : String >>
val x : IntOrString = << IntValue = 42 >>
val y : IntOrString = << StringValue = "qwerty" >>
```

Similar to records, variants are structurally typed.

A _match_ expression can be used to identify which field of a variant value is
valid:

```
def getIntOrElseZero(x : IntOrString) Int =
    x match {
        case IntValue(n) => n
        case StringValue(_) => 0
    }
```

The compiler will enforce that the cases of a match expression are exhaustive.

An example of a common use for variants is the `Option` type:

```
def Option(T : Type) Type = << Some : T, None : Unit >>
```

In fact, even the `Boolean` type in Cooma is a variant type:

```
type Boolean = << True : Unit, False : Unit >>
```

## Vectors

Vectors are a built-in ordered collection type:

```
[42, 100, 16]
> res3 : Vector(Int) = [42, 100, 16]
```

Built-in vector operations are available in the prelude:

```
val v = [42, 100, 16]
> v : Vector(Int) = [42, 100, 16]

Vectors.append(Int, v, 1)
> res4 : Vector(Int) = [42, 100, 16, 1]

Vectors.concat(Int, v, [1, 2, 3])
> res5 : Vector(Int) = [42, 100, 16, 1, 2, 3]

Vectors.get(Int, v, 2)
> res6 : Int = 16

Vectors.prepend(Int, v, 1)
> res7 : Vector(Int) = [1, 42, 100, 16]

Vectors.put(Int, v, 1, 7)
> res8 : Vector(Int) = [42, 7, 16]
```
