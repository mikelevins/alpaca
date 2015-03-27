# bard  reference
version 0.4

Alpaca's built-in Lisp

## Named literals

| Name | Description
| -
| `true` | the Boolean true value
| `false` | the Boolean false value
| `nothing` | an empty collection
| `end` | the end of a stream
| `undefined` | no useful value

## Classes

Bard's abstract types

| Name | Description
| -
|`    Anything`         | All values
|`      Stream`         | Objects you can read from or write to
|`      Container`     | Objects that contain groups of other objects
|`       Map `          | Containers of key/value pairs
|`        List`         | Ordered, countable collections
|`          Pair`       | Lists with two elements, a left one and a right one
|`        Array`        | Containers of objects arranged in rows and columns
|`          Vector`     | Arrays with only rows (Vectors are also Lists)
|`            String`   | Vectors of text characters
|`              Text`   | Editable texts used in Alpaca documents
|`      Atom`           | Objects that are not collections
|`        Type`         | Objects that represent the types of other objects, like List and Text
|`        Procedure`    | Objects that can be executed to perform computations
|`        Name`         | Objects used to name other objects
|`        Character`    | Individual text characters
|`        Condition`    | Objects that represent significant things that happen while a program runs
|`          Event`      | Conditions that represent things users can do, such as clicking a mouse button
|`        Unique`       | Objects that are unique, like the named literals
|`          Boolean`    | true and false
|`        Number`       | Mathematical values
|`          Complex`    | Numbers with imaginary parts
|`          Real`       | Numbers without imaginary parts
|`            Float`    | Inexact decimalß numbers
|`            Rational` | Exact ratios
|`            Integer`  | Whole numbers
|`              Byte`   | 8-bit computer words

## Structures

Bard's concrete types

| Name | Description
| -
|character       | 
|class           |
|complex-number  |
|cons            |
|float           |
|function        |
|hash-table      |
|integer         |
|method          |
|ratio           |
|string          |
|symbol          |
|text            |
|treelist        |
|treemap         |
|uri             |
