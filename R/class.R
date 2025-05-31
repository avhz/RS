## ============================================================================
## Class
## ============================================================================

#' @title Class
#'
#' @description Create a new ClassInstance in R.
#'
#' @details
#' The Class function creates a new ClassInstance in R.
#' It allows you to define fields and methods for the class.
#'
#' @param name The name of the class.
#' @param ... The fields and methods of the class.
#'
#' @export
Class <- function(.classname, ...) {
    .self <- .Call(
        "wrap__ClassDefinition__new",
        name = .classname,
        methods = list(...),
        PACKAGE = "RS"
    )

    new_class <- function(...) {
        .Call(
            "wrap__ClassInstance__new",
            name = .classname,
            fields = rlang::list2(...),
            def = .self,
            PACKAGE = "RS"
        )
    }

    assign(.classname, new_class, envir = parent.frame())
}

#' @export
print.ClassInstance <- function(x, ...) {
    .print_rust_object(x)
}

#' @export
.DollarNames.ClassInstance <- function(env, pattern = "") {
    ls(ClassInstance, pattern = pattern)
}

#' @export
`@.ClassInstance` <- function(self, name) {
    .attr <- .Call("wrap__ClassInstance__get", self, name)
    if (is.function(.attr)) return(function(...) .attr(self, ...))
    return(.attr)
}

#' @export
`@<-.ClassInstance` <- function(self, name, value) {
    .Call("wrap__ClassInstance__set", self, name, value)
    return(self)
}

#' @export
print.extendr_error <- function(error) {
    print(error$value)
}

if (FALSE) {
    . <- function() {
        gc()
        remove(list = ls())
        rextendr::clean()
        rextendr::document()
        devtools::load_all()
        devtools::test()
    }
    .()

    (bm <- .benchmark(1e4))
    ggplot2::autoplot(bm)

    Class(
        "Foo",
        a = t_int,
        b = t_dbl,
        c = t_char
    )

    Foo(a = 1L, b = 2.0, c = "xxx")
    Foo(a = 1L, b = 2.0, c = 1)

    system.time(for (i in 1:1e6) Foo(a = 1L, b = 2.0, c = "xxx"))
}
