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
Class1 <- function(.classname, ...) {
    # browser()
    definition_args <- list(...)

    # methods <- names(Filter(
    #     \(.f) is.function(.f) && (".self" %in% formalArgs(.f)),
    #     definition_args
    # ))
    # if (is.null(methods)) methods <- list()

    .self <- .Call(
        "wrap__ClassDefinition__new",
        name = .classname,
        # methods = as.list(methods),
        methods = definition_args,
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

    Class1(
        "FooRS1",
        a = t_int,
        b = t_dbl,
        c = t_char,

        bar = function(y) print(y)
    )

    `@.ClassInstance` <- function(self, name) self$get(name)
    `@<-.ClassInstance` <- function(self, k, v) self$set(k, v)

    foo1 <- FooRS1(a = 1L, b = 2.0, c = "xxx")
    foo1
    foo1@a
    foo1@b <- 1111
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

    foo1$get("a")
    foo2$get("a")

    system.time(
        for (i in 1:1e6) {
            # Class("Foo", a = t_int)
            . <- .Call(wrap__ClassMap__new)
        }
    )

    system.time(
        for (i in 1:1e6) {
            # Class("Foo", a = t_int)
            . <- .Call(wrap__ClassMap__new)
        }
    )

    bench::mark(
        # ClassMap$from_list(list(x = 1L, y = 2.0, z = "hello")),
        # ClassMap$with_capacity(10L),
        # ClassMap$new(),
        # "cm" = .Call(wrap__ClassMap__new),
        "ext" = RSClass$define("Foo", list()),
        "rscl" = .Call(wrap__RSClass__define, "Foo", list()),

        iterations = 1e6
    )

    Class("Foo", a = t_int)

    foo1 <- Foo(a = 1L)
    foo2 <- Foo(a = 2L)

    foo1
    foo2

    system.time(for (i in 1:1e6) Foo(1L, 2.0, "xxx"))

    "Foo" %class%
        c(
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
