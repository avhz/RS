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

    methods <- names(Filter(
        \(.f) is.function(.f) && (".self" %in% formalArgs(.f)),
        definition_args
    ))
    if (is.null(methods)) methods <- character(0)

    .self <- .Call(
        "wrap__define_class",
        name = .classname,
        definition_args = definition_args,
        methods = methods,
        PACKAGE = "RS"
    )

    new_class <- function(...) {
        .Call(
            "wrap__initialise_class",
            name = .classname,
            self_ = .self,
            instance_args = rlang::list2(...),
            PACKAGE = "RS"
        )
    }

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
`@.RS_CLASS` <- function(self, key) {
    self[["map"]][["get"]](key)
}

#' @export
`@.RS_SELF` <- function(self, key) {
    self[["map"]][["get"]](key)
}

#' @export
`@<-.RS_CLASS` <- function(self, key, value) {
    self[["map"]][["set"]](key, value)
    return(self)
}

#' @export
`@<-.RS_SELF` <- function(self, key, value) {
    self[["map"]][["set"]](key, value)
    return(self)
}


#' @export
print.RS_CLASS <- function(self, ...) {
    .get_width <- function(df) max(nchar(capture.output(print(df))))
    .get_signature <- function(fn) {
        paste0("f", "(", paste(formalArgs(fn), collapse = ", "), ")")
    }

    fields <- self[["map"]][["keys"]]() # !in c("new", "print")
    values <- self[["map"]][["values"]]()
    values <- unlist(lapply(
        values,
        \(v) if (is.function(v)) .get_signature(v) else v
    ))
    types <- sapply(fields, \(n) typeof(self[["map"]][["get"]](n)))

    df <- data.frame(field = fields, type = types, value = values)
    df <- df[order(df$field), ]

    width <- .get_width(df)
    sep <- rep("-", width) |> paste(collapse = "")
    cat(sep, fill = TRUE)
    cat(class(self), fill = TRUE)
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

    Class(
        "FooRS",
        a = t_int,
        b = t_dbl,
        c = t_char
    )

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
            a = S7::class_integer,
            b = S7::class_numeric,
            c = S7::class_character
        )
    )

    FooS4 <- setClass(
        "FooS4",
        slots = list(
            a = "integer",
            b = "numeric",
            c = "character"
        )
    )

    gc()

    bench::mark(
        "RS" = FooRS(a = 1L, b = 2.0, c = "xxx"),
        "R6" = FooR6$new(a = 1L, b = 2.0, c = "xxx"),
        "S4" = FooS4(a = 1L, b = 2.0, c = "xxx"),
        "S7" = FooS7(a = 1L, b = 2.0, c = "xxx"),
        "Ref" = FooRef(a = 1L, b = 2.0, c = "xxx"),

        iterations = 1e4,
        check = FALSE
    )

    # print(timings, width = Inf)
    # View(dplyr::mutate(timings, iter_gc = .data$`itr/sec` / .data$n_gc))

    system.time(for (i in 1:1e5) FooRS(1L, 2.0, "xxx"))

    Class(
        "Foo",
        x = t_int,
        bar = function(.self, y) cat(.self@x, y, "\n"),
        baz = function(z) cat(z, "\n")
    )

    foo1 <- Foo(x = 1L)
    foo2 <- Foo(x = 2L)
    foo1
    foo2

    foo <- Foo(1L)
    foo$map
    foo
    foo@x
    foo@x <- 2L
    foo@x
    foo@bar(3L)
    foo@baz(4L)

    bar <- function(.self, y) cat(.self@x, y, "\n")
    bar(foo, 2L)
}
