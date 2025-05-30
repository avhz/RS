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
    system.time(for (i in 1:1e6) . <- FooRS1(a = 1L, b = 2.0, c = "xxx"))

    Class(
        "FooRS1",
        a = t_int,
        b = t_dbl,
        c = t_char,

        bar = function(y) print(y),
        baz = function(self, something) {
            print(self@a)
            print(something)
        }
    )

    foo1 <- FooRS1(a = 1L, b = 2.0, c = "xxx")
    foo1
    foo1@a
    foo1@b <- 1111
    foo1@b
    foo1@bar()
    foo1@baz(123)

    foo1$print()
    foo1$get("bar")(3L)
    foo1$get("a")
    foo1$set("a", 3L)
    foo1$get("a")

    foo2 <- FooRS1(a = 1L, b = 2.0, c = "xxx")
    foo2
    foo2$print()
    foo2$get("bar")(3L)
    foo2$get("a")
    foo2$set("a", 420L)
    foo2$get("a")

    foo1@a
    foo2@a
}
