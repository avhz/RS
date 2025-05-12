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
Class <- function(.classname, .validate = TRUE, ...) {
    definition_args <- list(...)
    .is_method <- function(.f) is.function(.f) && (".self" %in% formalArgs(.f))
    methods <- Filter(.is_method, definition_args) |> names()

    new <- function(...) {
        .Call(
            wrap__new_class,
            .classname,
            .validate,
            definition_args,
            rlang::list2(...), ## Instance fields
            if (!is.null(methods)) methods else character(0)
        )
    }

    assign(.classname, new, envir = parent.frame())
}

#' @export
print.ClassMap <- function(x, ...) .print_rust_object(x)

#' @export
.DollarNames.ClassMap <- function(env, pattern = "") {
    ls(ClassMap, pattern = pattern)
}

#' @export
`@.Class` <- function(self, key) self[["map"]][["get"]](key)

#' @export
`@.Self` <- function(self, key) self[["map"]][["get"]](key)

#' @export
`@<-.Class` <- function(self, key, value) {
    self[["map"]][["set"]](key, value)
    return(self)
}

#' @export
`@<-.Self` <- function(self, key, value) {
    self[["map"]][["set"]](key, value)
    return(self)
}


#' @export
print.Class <- function(self, ...) {
    .get_width <- function(df) max(nchar(capture.output(print(df))))

    fields <- self[["map"]][["keys"]]() # !in c("new", "print")
    values <- self[["map"]][["values"]]()
    ## Only keep values that are not functions
    values <- unlist(lapply(values, \(v) if (is.function(v)) "-" else v))
    types <- sapply(fields, \(n) typeof(self[["map"]][["get"]](n)))
    df <- data.frame(field = fields, type = types, value = values)

    df <- df[order(df$field), ]

    width <- .get_width(df)
    sep <- rep("-", width) |> paste(collapse = "")
    cat(sep, "\n")
    cat("Class:", self$.classname, "\n")
    cat(sep, "\n")
    df |> print(row.names = FALSE, right = FALSE)
    cat(sep, "\n")
}


if (FALSE) {
    gc()
    remove(list = ls())
    # rextendr::clean()
    rextendr::document()
    devtools::load_all()
    devtools::test()

    N <- 1e+5

    ## Without validation
    Class("FooValidated", TRUE, a = t_int, b = t_dbl, c = t_char)
    Class("FooUnvalidated", a = t_int, b = t_dbl, c = t_char, .validate = FALSE)

    FooR6 <- R6::R6Class(
        "FooR6",
        public = list(
            a = NULL,
            b = NULL,
            c = NULL,

            initialize = function(a, b, c) {
                self$a <- a
                self$b <- b
                self$c <- c
            }
        )
    )

    FooRef <- setRefClass(
        "FooRef",
        fields = list(a = "integer", b = "numeric", c = "character")
    )

    bench::mark(
        FooValidated(a = 1L, b = 2.0, c = "xxx"),
        FooUnvalidated(a = 1L, b = 2.0, c = "xxx"),
        FooR6$new(a = 1L, b = 2.0, c = "xxx"),
        FooRef$new(a = 1L, b = 2.0, c = "xxx"),

        iterations = N,
        check = FALSE
    )

    Class(
        .classname = "Foo",

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

    foo <- Foo(a = 1L, b = 2.0, c = "xxx")
    foo <- Foo(a = 1L, b = 2.0, c = NULL)
    n <- 1e+5
    bench::mark(Foo(a = 1L, b = 2.0, c = "xxx"), iterations = n)
    system.time(foos <- lapply(1:n, \(i) Foo(a = i, b = 1.5, c = "xxx")))
    profvis::profvis(lapply(1:n, \(i) Foo(a = i, b = 1.5, c = "xxx")))

    foo@a
    foo@b
    foo@c
    foo@c <- "new value"
    foo@c
    foo@bar(1)
    foo@bar(1, 2)
    foo@baz(1, 2)
    foo@baz(1, 2, data.frame(x = 1:5))

    FooR6 <- R6::R6Class(
        "FooR6",
        public = list(
            a = NULL,
            b = NULL,
            c = NULL,

            initialize = function(a, b, c) {
                self$a <- a
                self$b <- b
                self$c <- c
            },

            bar = function(x) {
                cat("Arg 'x' is", x, "\n")
                cat("Field 'a' is", self$a, "\n")
                cat("Field 'b' is", self$b, "\n")
                cat("Field 'c' is", self$c, "\n")
                cat("Updating field 'c' to 'new value'\n")
                self$c <- "new value"
                print("Calling method 'baz'")
                self$baz(1, 2)
            },

            baz = function(a, b, c = data.frame(x = 1:5)) {
                cat("Arg 'a' is", a, "\n")
                cat("Arg 'b' is", b, "\n")
                print(c)
            }
        )
    )

    fooR6 <- FooR6$new(a = 1L, b = 2.0, c = "xxx")

    bench::mark(
        FooR6$new(a = 1L, b = 2.0, c = "xxx"),
        iterations = n
    )
    system.time(fooR6 <- lapply(1:n, \(i) FooR6$new(a = i, b = 1.5, c = "xxx")))
}
