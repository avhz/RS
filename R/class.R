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

    methods <- names(Filter(
        \(.f) is.function(.f) && (".self" %in% formalArgs(.f)),
        definition_args
    ))
    if (is.null(methods)) methods <- character(0)

    .self <- .Call(
        wrap__define_class,
        .classname,
        definition_args,
        methods
    )

    new_class <- .compile(function(...) {
        .Call(
            wrap__new_class,
            .classname,
            .validate,
            .self,
            rlang::list2(...)
        )
    })

    assign(.classname, new_class, envir = parent.frame())
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
`@.Class` <- function(self, key) {
    self[["map"]][["get"]](key)
}

#' @export
`@.Self` <- function(self, key) {
    self[["map"]][["get"]](key)
}

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
    .get_signature <- function(nm, fn) {
        paste0(nm, "(", paste(formalArgs(fn), collapse = ", "), ")")
    }

    fields <- self[["map"]][["keys"]]() # !in c("new", "print")
    values <- self[["map"]][["values"]]()
    values <- unlist(lapply(
        values,
        \(v) if (is.function(v)) .get_signature("", v) else v
    ))
    types <- sapply(fields, \(n) typeof(self[["map"]][["get"]](n)))

    df <- data.frame(field = fields, type = types, value = values)
    df <- df[order(df$field), ]

    width <- .get_width(df)
    sep <- rep("-", width) |> paste(collapse = "")
    cat(sep, fill = TRUE)
    cat(class(self), fill = T)
    cat(sep, fill = TRUE)
    df |> print(row.names = FALSE, right = FALSE)
    cat(sep, fill = TRUE)
}


if (FALSE) {
    gc()
    remove(list = ls())
    rextendr::clean()
    rextendr::document()
    devtools::load_all()
    devtools::test()

    ## Without validation
    Class("FooValidated", TRUE, a = t_int, b = t_dbl, c = t_char)
    Class("FooUnvalidated", FALSE, a = t_int, b = t_dbl, c = t_char)

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

    FooS7 <- S7::new_class(
        "FooS7",
        properties = list(
            x = S7::class_integer,
            y = S7::class_integer,
            z = S7::class_integer
        )
    )

    system.time(for (i in 1:1e5) FooValidated(1L, 2.0, "xxx"))

    timings <- bench::mark(
        FooUnvalidated(a = 1L, b = 2.0, c = "xxx"),
        FooValidated(a = 1L, b = 2.0, c = "xxx"),
        FooR6$new(a = 1L, b = 2.0, c = "xxx"),
        FooRef$new(a = 1L, b = 2.0, c = "xxx"),
        FooS7(x = 1L, y = 2L, z = 3L),

        iterations = 1e4,
        check = FALSE
    )

    timings |>
        dplyr::mutate(iter_gc = .data$`itr/sec` / n_gc) |>
        View()

    Class(
        "Foo",

        x = t_int,
        bar = function(.self, y) cat(.self@x, y, "\n"),
        baz = function(z) cat(z, "\n")
    )

    foo <- Foo(x = 1L)
    foo@x <- 2L
    foo@bar(3L)
    foo@baz(4L)

    bar <- function(.self, y) cat(.self@x, y, "\n")
    bar(foo, 2L)
}
