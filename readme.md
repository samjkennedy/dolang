# Do

Do is an interpreted functional, statically typed, stack based, concatenative programming language. 

## Introduction

### The Stack

Do is a stack based language, meaning that programs consist of a series of operations that manipulate a stack. For example the simple program

```
4 5 * print
```

pushes the numbers 4 and 5 onto the stack, then `*` pops the top two elements from the stack and pushes their product back onto the stack, `print` pops the top value from the stack and prints it to the console.

Do is heavily inspired by the type safety of [Porth](https://gitlab.com/tsoding/porth) and features a compile time type checker to ensure the correctness of the program. For instance if the above program were instead:

```
true 5 * print
```

Do would know at compile time that `*` is trying to pop an integer and a boolean from the stack and will print a helpful error message:

```
ERROR: Type mismatch, expected 'int' but the top of the stack was `bool` at .\examples\example.do:1:8
1:    true 5 * print

bool introduced at .\examples\example.do:1:1
1:    true 5 * print
```

Additionally Do will surface when the stack will not be empty at the end of execution, as it is assumed all well-formed programs will entirely consume the stack on completion.

```
5 6 *
```

```
ERROR: Stack was not empty at the end of the file.
Stack: [Int]
[0] int introduced at .\examples\example.do:1:5
1:    5 6 *
```

### Functional Programming

Do allows the pushing of arbitrary functions to the stack, surround a sequence of operations with `[]` to do so:

```
[dup *]
```
This does not run the function but the top of the stack now points at a function that will duplicate the top of the stack and multiply it with itself. This is the square function. It can be applied to a list of integers with the `map` intrinsic:

```
[1 2 3 4 5] [dup *] map
```

After executing map, the stack will contain a list of the first 5 square numbers, they can be printed to the console with the `apply` intrinsic:

```
[1 2 3 4 5] 
    [dup *] map
    [print] apply
```

```
25
16
9
4
1
```

`apply` and `map` both expect a `list of any` and a `fn any -- any` to be on the top of the stack, with map leaving the mapped list on the stack and apply consuming the values. Do will also type check functions at compile time, for instance if we tried squaring a list of `bool`:

```
[true false true false false] 
    [dup *] map
    [print] apply
```

```
ERROR: Type mismatch, expected 'list of int' but the top of the stack was `list of bool` at .\examples\example.do:2:13
2:        [dup *] map

list of bool introduced at .\examples\example.do:1:1
1:    [true false true false false] 
```

### Intrinsics

#### Stack Manipulation

| Name    | Signature        | Description                                                        |
| ------- | ---------------- | ------------------------------------------------------------------ |
| `print` | `a b -- a`       | prints the value on top of the stack and remove it from the stack. |
| `swap`  | `a b -- b a`     | swaps the 2 values on top of the stack.                            |
| `pop`   | `a b -- a`       | pops the value on top of the stack.                                |
| `dup`   | `a -- a a`       | duplicates the value on top of the stack.                          |
| `over`  | `a b -- a b a`   | copies the value below the top of the stack                        |
| `rot`   | `a b c -- b c a` | rotates the top three stack values.                                |

#### List Functions

| Name      | Signature                    | Description                                                                                                              |
| --------- | ---------------------------- | ------------------------------------------------------------------------------------------------------------------------ |
| `map`     | `[a] fn(a -- b) -- [b]`      | maps every element of the list below the top of the stack with the function on top of the stack.                         |
| `apply`   | `[a] fn(a --) --`            | applies the function on the top of the stack to the list below, removing both from the stack.                            |
| `filter`  | `[a] fn(a -- bool) -- [a]`   | filters the list below the top of the stack by the function on top of the stack.                                         |
| `fold`    | `[a] fn(a a -- b) b -- b`    | reduces the list 2 below the stack using the reduce function below the top into the accumulator at the top of the stack. |
| `len`     | `[a] -- int`                 | pops the list off the top of the stack and pushes its length.                                                            |
| `concat`  | `[a] [a] -- [a]`             | concatenates two lists of the same type on the top of the stack into a single list.                                      |
| `head`    | `[a] -- a`                   | Pops the list on top of the stack and pushes its top element onto the stack.                                             |
| `last`    | `[a] -- a`                   | Pops the list on top of the stack and pushes its bottom element onto the stack.                                          |
| `tail`    | `[a] -- [a]`                 | Pops the list on top of the stack and pushes a list containing all its elements but the top.                             |
| `init`    | `[a] -- [a]`                 | Pops the list on top of the stack and pushes a list containing all its elements but the bottom.                          |
| `cons`    | `a a -- [a]`                 | Pops two values from the top of the stack and pushes a list containing those two elements.                               |
| `sort`    | `[int\|bool] -- [int\|bool]` | Pops the list on top of the stack and pushes it sorted. Only applicable to int lists and bool lists for now.             |
| `reverse` | `[int\|bool] -- [int\|bool]` | Pops the list on top of the stack and pushes it reversed. Only applicable to int lists and bool lists for now.           |

### Functions

Functions are declared with the `define` keyword, followed by the function name and its input and output types. Then the body of the function terminated with the `end` keyword.

```
define square (int -- int)
    dup *
end
```

The function can then be called by pushing its arguments onto the stack and pushing the function name:

```
5 square print
```

```
> 25
```

The function is type checked at compile time, so if you mistakenly pop too many values or the wrong types of values, or put too many values onto the stack at the end, you will get a compile time error.

```
define square (-- int)
    dup *
end
```
```
ERROR: Expected 'any' but the stack was empty at .\examples\example.do:2:5
2:        dup *
```


```
define square ( int -- bool )
    dup *
end
```
```
ERROR: Type mismatch, expected 'bool' but the top of the stack was `int` at .\examples\example.do:1:1
1:    define square ( int -- bool )

int introduced at .\examples\example.do:2:9
2:        dup *
```

Once defined, functions can be used in conjunction with any of the intrinsics, so long as they satisfy their signatures:

```
define square ( int -- int )
    dup *
end

[1 2 3 4 5]
    [square] map
    [print] apply
```

```
25
16
9
4
1
```

## Examples

Finding the greatest sum of a list of integers and printing it:

```
define flatten (list of list of any -- list of any)
    [concat] [] fold
end

define sum (list of int -- int)
    [+] 0 fold
end

[[1 2] [1 3]]
    [sum] map
    flatten
    [max] 0 fold
    print
```