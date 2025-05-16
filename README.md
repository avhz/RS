# `RS` - Faster classes for R

The name `RS` is a combination of R and S (R's predecessor), 
and is the file extension and top-level domain (`.rs`) 
used for Rust files and websites, respectively. 

## Install

Any of the following should work:

```R
pak::pak("avhz/RS")
## or
devtools::install_github("avhz/RS")
## or
renv::install("avhz/RS")
```

## Benchmark

```R
expression     min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
RS          4.26µs  5.04µs   191415.        0B     19.1  9999     1     52.2ms
R6         12.34µs 12.96µs    75757.        0B     83.4  9989    11    131.9ms
S4          32.1µs 33.58µs    28526.      328B     65.8  9977    23    349.7ms
S7         45.18µs 46.99µs    20296.        0B     63.1  9969    31    491.2ms
Ref        94.05µs 97.25µs     9971.    47.4KB     40.0  9960    40    998.9ms
```

Benchmark code [here](https://github.com/avhz/RS/blob/main/bench.R).

Note: This is not really a fair "benchmark", 
since RS does not yet have as many features as some of the other OOP offerings. 

## Examples

```R
Class(
    "Foo",
    # Fields
    a = t_int,
    b = t_dbl,
    c = t_char,

    # Methods
    bar = function(.self, x) {
        cat("Arg 'x' is", x, "\n")
        cat("Field 'a' is", .self@a, "\n")

        .self@c <- "new value"

        .self@baz(1, 2)
    },

    # Static methods
    baz = function(a, b, c = data.frame(x = 1:5)) {
        cat("Arg 'a' is", a, "\n")
        cat("Arg 'b' is", b, "\n")
        print(c)
    }
)
```

```R
> foo <- Foo(a = 1L, b = 2.0, c = "xxx")
> foo@a
[1] 1
> foo@b
[1] 2
> foo@c
[1] "xxx"
> foo@bar(1)
Arg 'x' is 1 
Field 'a' is 1 
Arg 'a' is 1 
Arg 'b' is 2 
  x
1 1
2 2
3 3
4 4
5 5
> foo@baz(1, 2)
Arg 'a' is 1 
Arg 'b' is 2 
  x
1 1
2 2
3 3
4 4
5 5
> foo@c
[1] "new value"
```

```R
Class(
    "Black76",

    ## Fields
    F = t_dbl,
    K = t_dbl,
    T = t_dbl,
    r = t_dbl,
    v = t_dbl,

    ## Methods
    call = function(.self) {
        .self@.df() *
            (pnorm(.self@.d1()) * .self@F - pnorm(.self@.d2()) * .self@K)
    },
    put = function(.self) {
        .self@.df() *
            (pnorm(-.self@.d2()) * .self@K - pnorm(-.self@.d1()) * .self@F)
    },
    .df = function(.self) {
        exp(-.self@r * .self@T)
    },
    .d1 = function(.self) {
        (log(.self@F / .self@K) + 0.5 * (.self@v)^2 * .self@T) /
            (.self@v * sqrt(.self@T))
    },
    .d2 = function(.self) {
        (log(.self@F / .self@K) - 0.5 * (.self@v)^2 * .self@T) /
            (.self@v * sqrt(.self@T))
    }
)

black76 <- Black76(F = 100, K = 100, T = 1, r = 0.05, v = 0.2)

black76@call()
black76@put()
```

Or create from a `data.frame`:

```R
Class("Asset",

    # FIELDS
    id = t_char,
    company = t_char,
    type = t_char,
    price = t_dbl,
    quantity = t_int,

    # METHODS
    value = function(.self) {
        .self@price * .self@quantity
    },
    print = function(.self) {
        cat("Asset ID:", .self@id, "\n")
        cat("Name:", .self@company, "\n")
        cat("Type:", .self@type, "\n")
        cat("Price:", .self@price, "\n")
        cat("Quantity:", .self@quantity, "\n")
        cat("Total Value:", .self@value(), "\n")
    }
)

asset <- Asset(
    id = "AAPL",
    company = "Apple Inc.",
    type = "Equity",
    price = 150.0,
    quantity = 10L,
)

df <- data.frame(
    id = c("AAPL", "GOOGL", "AMZN"),
    company = c("Apple Inc.", "Alphabet Inc.", "Amazon.com Inc."),
    type = c("Equity", "Equity", "Equity"),
    price = c(150.0, 2800.0, 3400.0),
    quantity = c(10L, 5L, 2L)
)

assets <- lapply(seq_len(nrow(df)), function(i) Asset(!!!df[i, ]))
```

```R
assets
[[1]]
-------------------------------------- 
Class: 
-------------------------------------- 
 field    type      value     
 price    double    150       
 value    closure   -         
 company  character Apple Inc.
 print    closure   -         
 quantity integer   10        
 id       character AAPL      
 type     character Equity    
-------------------------------------- 

[[2]]
----------------------------------------- 
Class: 
----------------------------------------- 
 field    type      value        
 value    closure   -            
 print    closure   -            
 type     character Equity       
 quantity integer   5            
 price    double    2800         
 company  character Alphabet Inc.
 id       character GOOGL        
----------------------------------------- 

[[3]]
------------------------------------------- 
Class: 
------------------------------------------- 
 field    type      value          
 price    double    3400           
 company  character Amazon.com Inc.
 value    closure   -              
 quantity integer   2              
 print    closure   -              
 type     character Equity         
 id       character AMZN           
------------------------------------------- 
```
