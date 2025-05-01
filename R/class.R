## ============================================================================
## Class
## ============================================================================

#' @title Class
#'
#' @description Create a new class
#'
#' @details
#' The Class function creates a new class in R.
#' It allows you to define fields and methods for the class.
#'
#' @param name The name of the class.
#' @param ... The fields and methods of the class.
#'
#' @export
Class <- function(name, ...) {
    definition_args <- list(...)
    .is_method <- function(.f) is.function(.f) && (".self" %in% formalArgs(.f))
    methods <- Filter(.is_method, definition_args)

    new <- function(...) {
        instance_args <- rlang::list2(...)

        .Call(
            wrap____new_class__,
            name,
            definition_args,
            instance_args,
            methods
        )
    }

    assign(name, new, envir = parent.frame())
}

#' @export
`@.Class` <- function(self, key) {
    self[["map"]][["get"]](key)
}

#' @export
`@<-.Class` <- function(self, key, value) {
    self$map[["insert"]](key, value)
    return(self)
}

#' @export
`@.Self` <- function(self, key) {
    self[["map"]][["get"]](key)
}

#' @export
`@<-.Self` <- function(self, key, value) {
    self$map[["set"]](key, value)
    return(self)
}

.DollarNames.Class <- function(env, pattern = "") {
    ls(Class, pattern = pattern)
}

#' @export
print.Class <- function(self, ...) {
    .get_width <- function(df) max(nchar(capture.output(print(df))))

    fields <- self$map$keys() # !in c("new", "print")
    values <- self$map$values()
    ## Only keep values that are not functions
    values <- unlist(lapply(values, \(v) if (is.function(v)) "-" else v))
    types <- sapply(fields, \(n) typeof(self$map$get(n)))
    df <- data.frame(field = fields, type = types, value = values)

    width <- .get_width(df)
    sep <- rep("-", width) |> paste(collapse = "")
    cat(sep, "\n")
    cat("Class:", self$name, "\n")
    cat(sep, "\n")
    df |> print(row.names = FALSE, right = FALSE)
    cat(sep, "\n")
}


if (FALSE) {
    gc()
    remove(list = ls())
    rextendr::document()
    devtools::load_all()

    Class(
        "Foo",

        ## Fields
        a = t_int,
        b = t_dbl,
        c = t_char,

        ## Methods
        bar = function(.self, x) {
            cat("Arg 'x' is", x, "\n")
            cat("Field 'a' is", .self@a, "\n")
            cat("Field 'b' is", .self@b, "\n")
            cat("Field 'c' is", .self@c, "\n")
            cat("Updating field 'c' to 'new value'\n")
            .self@c <- "new value"
            print("Calling method 'baz'")
            .self@baz(1, 2)
        },

        ## Static methods
        baz = function(a, b, c = data.frame(x = 1:5)) {
            cat("Arg 'a' is", a, "\n")
            cat("Arg 'b' is", b, "\n")
            print(c)
        }
    )

    bench::mark(foo <- Foo(a = 1L, b = 2.0, c = "xxx"))

    foo@a
    foo@b
    foo@c
    foo@baz(1, 2)
    foo@bar(10)
    foo@c

    n <- 1e+5
    system.time(foos <- replicate(n, Foo(a = 1L, b = 1.5, c = "xxx")))
    # profvis::profvis(lapply(1:n, \(i) Foo(a = i, b = 1.5, c = "xxx")))

    foo2 <- Foo(a = 1L, b = 2.0) ## HANDLE MISSING ARGUMENTS

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

    black76 <- Black76(F = 55, K = 100, T = 1, r = 0.05, v = 0.2)

    black76@call()
    black76@put()

    Class("Foo", a = t_int, b = t_dbl, c = t_char)
    Class("Bar", foo = Foo, baz = t_dbl)

    foo <- Foo(a = 1L, b = 2.0, c = "xxx")
    bar <- Bar(foo = foo, baz = 3.0)
    bar

    .is_method <- function(.f) is.function(.f) && (".self" %in% formalArgs(.f))

    .is_method(Foo)
    .is_method(foo)

    class(foo)
    class(Foo)
}
