# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text := new_class(parent = class_character)
Number := new_class(parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 := new_generic("x")
method(foo_S7, Text) <- function(x, ...) paste0(x, "-foo")

foo_S3 <- function(x, ...) {
  UseMethod("foo_S3")
}

foo_S3.Text <- function(x, ...) {
  paste0(x, "-foo")
}

library(methods)
setOldClass(c("Number", "numeric", "S7_object"))
setOldClass(c("Text", "character", "S7_object"))

setGeneric("foo_S4", function(x, ...) standardGeneric("foo_S4"))
#> [1] "foo_S4"
setMethod("foo_S4", c("Text"), function(x, ...) paste0(x, "-foo"))

# Measure performance of single dispatch
bench::mark(foo_S7(x), foo_S3(x), foo_S4(x))
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 foo_S7(x)    6.37µs   7.14µs   131665.    10.9KB     26.3
#> 2 foo_S3(x)    2.58µs   2.92µs   309461.        0B     30.9
#> 3 foo_S4(x)    2.78µs   3.16µs   302526.        0B     30.3

bar_S7 := new_generic(c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)  13.46µs  14.72µs    64968.        0B     19.5
#> 2 bar_S4(x, y)   7.32µs   8.48µs   114011.        0B     22.8
```

A potential optimization is caching based on the class names, but lookup
should be fast without this.

The following benchmark generates a class hierarchy of different levels
and lengths of class names and compares the time to dispatch on the
first class in the hierarchy vs the time to dispatch on the last class.

We find that even in very extreme cases (e.g. 100 deep hierarchy 100 of
character class names) the overhead is reasonable, and for more
reasonable cases (e.g. 10 deep hierarchy of 15 character class names)
the overhead is basically negligible.

``` r

library(S7)

gen_character <- function (n, min = 5, max = 25, values = c(letters, LETTERS, 0:9)) {
  lengths <- sample(min:max, replace = TRUE, size = n)
  values <- sample(values, sum(lengths), replace = TRUE)
  starts <- c(1, cumsum(lengths)[-n] + 1)
  ends <- cumsum(lengths)
  mapply(function(start, end) paste0(values[start:end], collapse=""), starts, ends)
}

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 := new_generic("x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic("x")
    method(foo2_S7, S7_object) <- function(x, ...) paste0(x, "-foo")

    bench::mark(
      best = foo_S7(x),
      worst = foo2_S7(x)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15   6.37µs   7.32µs   131601.        0B    13.2 
#>  2 worst                3          15   6.52µs   7.49µs   123737.        0B    24.8 
#>  3 best                 5          15   6.41µs    7.3µs   129877.        0B    26.0 
#>  4 worst                5          15   6.57µs   7.59µs   125589.        0B    25.1 
#>  5 best                10          15   6.41µs   7.34µs   130854.        0B    26.2 
#>  6 worst               10          15   6.83µs   7.75µs   123864.        0B    24.8 
#>  7 best                50          15   6.64µs   7.47µs   128235.        0B    25.7 
#>  8 worst               50          15   8.54µs   9.56µs   100531.        0B    20.1 
#>  9 best               100          15   6.88µs   7.75µs   124138.        0B    24.8 
#> 10 worst              100          15  10.76µs  11.77µs    81390.        0B    16.3 
#> 11 best                 3         100   6.59µs   7.51µs   125884.        0B    25.2 
#> 12 worst                3         100   6.93µs   7.83µs   122004.        0B    24.4 
#> 13 best                 5         100   6.66µs    7.6µs   126317.        0B    12.6 
#> 14 worst                5         100   6.99µs   7.89µs   120512.        0B    24.1 
#> 15 best                10         100   6.54µs   7.42µs   127805.        0B    25.6 
#> 16 worst               10         100   7.42µs   8.29µs   114646.        0B    22.9 
#> 17 best                50         100   6.66µs   7.51µs   126834.        0B    12.7 
#> 18 worst               50         100   12.2µs  13.16µs    72811.        0B    14.6 
#> 19 best               100         100   7.05µs   7.95µs   120604.        0B    12.1 
#> 20 worst              100         100   18.8µs  19.92µs    48179.        0B     9.64
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))
    y <- do.call(cls, list("ho"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 := new_generic(c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic(c("x", "y"))
    method(foo2_S7, list(S7_object, S7_object)) <- function(x, y, ...) paste0(x, y, "-foo")

    bench::mark(
      best = foo_S7(x, y),
      worst = foo2_S7(x, y)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15   9.06µs   10.2µs    93562.        0B    28.1 
#>  2 worst                3          15   9.36µs   10.6µs    89253.        0B    17.9 
#>  3 best                 5          15   9.06µs   10.2µs    91517.        0B    27.5 
#>  4 worst                5          15   9.42µs   10.7µs    88586.        0B    26.6 
#>  5 best                10          15   9.11µs   10.3µs    92221.        0B    18.4 
#>  6 worst               10          15   9.99µs   11.1µs    84130.        0B    25.2 
#>  7 best                50          15   9.45µs   10.7µs    88554.        0B    17.7 
#>  8 worst               50          15  13.21µs   14.5µs    64386.        0B    19.3 
#>  9 best               100          15   9.96µs   11.3µs    83617.        0B    16.7 
#> 10 worst              100          15  18.12µs   19.4µs    48834.        0B    14.7 
#> 11 best                 3         100   9.19µs   10.4µs    91063.        0B    27.3 
#> 12 worst                3         100  10.06µs   11.4µs    83006.        0B    16.6 
#> 13 best                 5         100   9.06µs   10.3µs    90642.        0B    27.2 
#> 14 worst                5         100  10.22µs   11.5µs    82639.        0B    16.5 
#> 15 best                10         100   9.07µs   10.3µs    91071.        0B    18.2 
#> 16 worst               10         100  11.44µs   12.7µs    74393.        0B    22.3 
#> 17 best                50         100   9.56µs   10.3µs    94164.        0B    18.8 
#> 18 worst               50         100  19.66µs   20.5µs    47357.        0B    14.2 
#> 19 best               100         100   9.81µs   10.6µs    89311.        0B    26.8 
#> 20 worst              100         100  30.35µs   31.4µs    30437.        0B     9.13
```
