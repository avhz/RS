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
Class <- function(.classname, ...) {
    definition_args <- list(...)
    .is_method <- function(.f) is.function(.f) && (".self" %in% formalArgs(.f))
    methods <- Filter(.is_method, definition_args)

    new <- function(...) {
        instance_args <- rlang::list2(...)

        .Call(
            wrap____new_class__,
            .classname,
            definition_args,
            instance_args,
            if (!is.null(names(methods))) names(methods) else character(0)
        )
    }

    assign(.classname, new, envir = parent.frame())
}

#' @export
print.ClassMap <- function(x, ...) {
    .print_rust_object(x)
}

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
    # browser()
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
    rextendr::document()
    devtools::load_all()
    devtools::test()

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

    system.time(foo <- Foo(a = 1L, b = 2.0, c = "xxx"))
    n <- 1e+5
    bench::mark(foo <- Foo(a = 1L, b = 2.0, c = "xxx"), iterations = n)
    system.time(foos <- lapply(1:n, \(i) Foo(a = i, b = 1.5, c = "xxx")))
    profvis::profvis(lapply(1:n, \(i) Foo(a = i, b = 1.5, c = "xxx")))
}
