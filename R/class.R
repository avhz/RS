## ============================================================================
## Class
## ============================================================================

#' @title
#' Define a new RS class.
#'
#' @description
#' Create a new ClassInstance in R.
#'
#' @details
#' The Class function creates a new ClassInstance in R.
#' It allows you to define fields and methods for the class.
#'
#' @param .classname The name of the class.
#' @param .validate Whether to validate the class attributes.
#'     Note: setting this to `FALSE` gives a decent performance boost.
#' @param ... The fields and methods of the class definition.
#'
#' @export
Class <- function(.classname, ...) {
    .attributes <- rlang::list2(...)

    .resolve_type_generators <- function(attr) {
        if (inherits(attr, "ClassTypeGenerator")) {
            return(attr())
        }
        return(attr)
    }

    .attributes <- lapply(.attributes, .resolve_type_generators)

    # .is_method <- function(attr) TRUE ## PLACEHOLDER
    # .is_field <- function(attr) TRUE ## PLACEHOLDER
    # .fields <- Filter(.is_field, .attributes)
    # .methods <- Filter(.is_method, .attributes)

    .self <- .Call(
        "wrap__ClassDefinition__new",
        name = .classname,
        methods = .attributes,
        PACKAGE = "RS"
    )

    new_class <- function(...) {
        .Call(
            "wrap__ClassInstance__new",
            fields = rlang::list2(...),
            def = .self,
            PACKAGE = "RS"
        )
    }

    assign(.classname, new_class, envir = parent.frame())
}


## ============================================================================
## PLAYGROUND
## ============================================================================

if (FALSE) {
    . <- function() {
        gc()
        remove(list = ls())
        # rextendr::clean()
        rextendr::document()
        devtools::load_all()
        devtools::test()
    }
    .()

    (bm <- .benchmark(1e4))
    .benchplot(bm)
    ggplot2::autoplot(bm)

    # png("benchmark.png", width = 800, height = 700)
    # .benchplot(bm)
    # dev.off()

    reticulate::py_run_string(
        "\
import time 
from dataclasses import dataclass

@dataclass
class Foo1:
    a: int

t = time.perf_counter()
foos = [Foo1(a=1) for _ in range(1_000_000)]
print(f'Python time: {time.perf_counter() - t:.2f} seconds')

print(foos[0])
"
    )

    Class(
        "Foo",
        a = t_int,
        b = private(t_dbl),

        ## Methods
        bar = private(function(self, x) {
            self@a + x * self@b
        }),
        qux = function(self) {
            print(class(self))
        },

        ## Static methods
        baz = static(function(self, x) {
            self + x
        })
    )

    bench::mark(Foo(a = 1L), iterations = 1e5)
    foo <- Foo(a = 1L, b = 2.0)
    foo@qux()
    foo@a
    foo@b
    foo@bar(2L)
    foo@baz(2L, 3L)
    Class("Foo", a = t_int, b = t_dbl)
    Class("Bar", foo = Foo)
    Class("Foo", a = t_int, b = t_dbl)
    foo <- Foo(a = 1L, b = 2.0)
    bar <- Bar(foo = foo)
    foo
    foo@a
    bar@foo@a
    bar@foo@b
}
