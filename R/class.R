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
    methods <- (Filter(.is_method, definition_args) |> names())
    methods <- if (!is.null(methods)) methods else character(0)

    new <- function(...) {
        .Call(
            wrap__new_class,
            .classname,
            .validate,
            definition_args,
            rlang::list2(...), ## Instance fields
            methods
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
        FooUnvalidated(a = 1L, b = 2.0, c = "xxx"),
        FooValidated(a = 1L, b = 2.0, c = "xxx"),
        FooR6$new(a = 1L, b = 2.0, c = "xxx"),
        FooRef$new(a = 1L, b = 2.0, c = "xxx"),

        iterations = 1e+4,
        check = FALSE
    )
}
