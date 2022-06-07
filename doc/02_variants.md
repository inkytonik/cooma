# Variants

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
