# Adding references

References are a simple way to add mutable data to functional programming
languages. A reference is a pointer to a single cell whose value can change as
the program executes.

We'll add support for the following operations:

-   `(ref e)` creates a reference with the initial value `e`.
-   `(deref r)` gets the value from a reference.
-   `(set-ref r e)` sets the value of the reference `r` to `e`. It should
    evaluate to `e`.

References should be displayed by the interpreter and runtime as `(ref e)`,
where `e` is the representation of the currently stored value.

## Starting point

We'll build off the stencil for homework 4.

## Adding references to the interpreter

In the interpreter, implement references using OCaml's built-in `ref`. Add a
`Ref` constructor to the `value` type, then add support for the reference
operations described above.

## Adding references to the compiler

The compiler should put references on the heap and use `0b001` as the tag for
runtime reference values. Don't forget to update the runtime!

## Testing

### Testing mutable data

To make testing mutable data easier, we've implemented a common lisp construct
in the interpreter and compiler: `do` blocks. `do` blocks take in a sequence of
expressions (at least 1), execute each of them in order, and evaluate to
whatever the last expression evaluates to. For instance,

```lisp
(let ((r (ref 1)))
  (do (set-ref r 2)
      (deref r)))
```

should evaluate to `2`.

### CSV

In addition to the usual testing scheme based on `*.lisp`, `*.out` and `*.err`
files, you can now include a CSV file of anonymous tests in
`examples/examples.csv`. The format of each line is either
`<PROGRAM>,<EXPECTED>` or `<PROGRAM>`, depending on whether or not you want to
specify an expected output. The CSV does not support tests that are expected to
error--for those, and for more complicated tests for programs that span multiple
lines, use file-based tests. Leading and trailing whitespace is stripped from
each of the CSV's cells.
