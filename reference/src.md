# Create a "src" object

`src()` is the standard constructor for srcs and `is.src()` tests.

## Usage

``` r
src(subclass, ...)

is.src(x)
```

## Arguments

- subclass:

  name of subclass. "src" is an abstract base class, so you must supply
  this value. `src_` is automatically prepended to the class name

- ...:

  fields used by object.

  These dots are evaluated with [explicit
  splicing](https://rlang.r-lib.org/reference/list2.html).

- x:

  object to test for "src"-ness.
