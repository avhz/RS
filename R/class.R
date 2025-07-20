## ============================================================================
## Class
## ============================================================================

#' @title
#' Define a new RS class.
#'
#' @description
#' Create a new `ClassDefinition` in R.
#'
#' @details
#' The Class function creates a new `ClassDefinition` in R.
#' It allows you to define fields and methods for the class.
#'
#' @param .classname The name of the class.
#' @param .validate Whether to validate the class attributes.
#'     Note: setting this to `FALSE` gives a slight performance boost.
#' @param ... The fields and methods of the class definition,
#'     supplied as pairs of names and types using the [:=] operator.
#'
#' @export
Class <- function(.classname, ..., .validate = TRUE) {
    assert_pairlist_arguments <- function(...) {
        .valid_classes <- c(.RS[[".pairlist"]])
        if (any(sapply(list(...), Negate(inherits), .valid_classes))) {
            stop(
                "Class attributes must be defined using `:=` operator.",
                call. = FALSE
            )
        }
    }

    assert_pairlist_arguments(...)
    attributes <- do.call(c, list(...))

    resolve_type_generators <- function(attr) {
        if (inherits(attr, .RS[[".typegen"]])) {
            return(attr())
        }
        return(attr)
    }

    self <- .Call(
        wrap__ClassDefinition__new,
        name = .classname,
        methods = lapply(attributes, resolve_type_generators),
        validate = .validate,
        PACKAGE = "RS"
    )

    new_class <- function(...) {
        .Call(
            wrap__ClassInstance__new,
            fields = list(...),
            def = self,
            PACKAGE = "RS"
        )
    }

    new_class <- .structure(
        new_class,
        c(.RS[[".definition"]], .classname),
        name = .classname,
        validate = .validate
    )
    assign(.classname, new_class, envir = parent.frame())
}


## ============================================================================
## PLAYGROUND
## ============================================================================

if (FALSE) {
    . <- function() {
        gc()
        remove(list = ls())
        rextendr::clean()
        devtools::document()
        devtools::check()
        devtools::build_readme()
        pkgdown::build_site()
        pkgdown::build_site_github_pages()
        rextendr::document()
        devtools::load_all()
        devtools::test()
    }
    .()

    Class(
        "Foo",

        a := t_int,

        ## Static method
        bar := static_(function(x) print(x)),

        ## Instance method
        baz := function(self) print(self@a)
    )

    foo <- Foo(a = 1L)
    foo
    foo@baz()
    foo@a
    foo@bar(12345)

    Foo@bar(12345)

    (bm <- .benchmark(1e4))
    .benchplot(bm)
    ggplot2::autoplot(bm)
    png("benchmark.png", width = 800, height = 600)
    .benchplot(bm)
    dev.off()

    Class("Foo1", a := private_(t_int))
    foo1 <- Foo1(a = 1L)

    Class("Foo2", a := t_int, b := t_dbl)

    foo2 <- Foo2(a = 1L, b = foo1)

    Class(
        "Foo",

        a := t_int,
        b := t_dbl,
        c := t_char,
        d := t_dataframe,

        bar := function(self, new_x) {
            self@a <- new_x
        }
    )

    devtools::load_all()
    bench::mark(
        private_(\(x) x),
        static_(\(x) x),
        is_private(\(x) x),
        is_static(\(x) x),
        inherits(\(x) x, "StaticMethod"),

        iterations = 1e4,
        check = FALSE
    )

    bench::mark(
        .structure(list(x = 1), "FooFoo"),
        structure_(list(x = 1), "FooFoo", list()),

        iterations = 1e4,
        check = FALSE
    )

    gc()
    remove(list = ls())
    rextendr::document()
    devtools::load_all()

    baz <- function(.obj, .new_x) {
        .obj@x <- .new_x
    }

    Class(
        "Foo",

        x := t_int,

        bar := function(self, new_x) {
            baz(self, new_x)
        }
    )

    foo <- Foo(x = 1L)
    foo
    foo@bar(555L)
    foo
    foo@xxx

    `<<` <- function(x, value) {
        x@x <- value
        invisible(x)
    }

    foo@bar("sdf")

    bench::mark(
        local({})
    )

    x <- 1L
    local({
        print(x)
    })
    e <- new.env()
    attr(e, "name") <- "test_env"
    environmentName(e)
}
