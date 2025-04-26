# RS

Classes for R.

## Example

```R
Class(
    "Black76",

    ## Fields
    F = is.double,
    K = is.double,
    T = is.double,
    r = is.double,
    v = is.double,

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
    id = is.character,
    company = is.character,
    type = is.character,
    price = is.double,
    quantity = is.integer,

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






