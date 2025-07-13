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
Class <- function(.classname, ..., .validate = TRUE) {
    .assert_pairlist_arguments(...)
    .attributes <- do.call(c, list(...))

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
        wrap__ClassDefinition__new,
        name = .classname,
        methods = .attributes,
        validate = .validate,
        PACKAGE = "RS"
    )

    new_class <- function(...) {
        .Call(
            wrap__ClassInstance__new,
            # fields = rlang::list2(...),
            fields = list(...),
            def = .self,
            PACKAGE = "RS"
        )
    }

    new_class <- structure(
        new_class,
        class = c("ClassDefinition", .classname),
        name = .classname,
        validate = .validate
    )
    assign(.classname, new_class, envir = parent.frame())
}

#' @export
`:=` <- function(lhs, rhs) {
    .valid <- c(
        "ClassTypeGenerator",
        "ClassDefinition",
        "function"
    )
    if (!any(class(rhs) %in% .valid)) {
        stop("`:=` can only be used with RS types.")
    }
    pl <- pairlist()
    pl[[deparse(substitute(lhs))]] <- rhs
    structure(pl, class = "ClassPairlist")
}

.assert_pairlist_arguments <- function(...) {
    if (any(sapply(list(...), Negate(inherits), "ClassPairlist"))) {
        stop(
            "Class attributes must be defined using `:=` operator.",
            call. = FALSE
        )
    }
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

    Class(
        "Foo",

        a := t_int,
        b := t_dbl,
        c := t_char,

        bar := \(.self) print(.self)
    )

    foo <- Foo(a = 1L, b = 2.0, c = "xxx")
    foo

    `<<<<<` <- 1

    cat(foo$to_json_string())

    rlang::env_print(foo)

    (bm <- .benchmark(1e4))
    .benchplot(bm)
    ggplot2::autoplot(bm)

    bench::mark(
        list("a" = 1L, "b" = "test"),
        rlang::list2(a = 1L, b = "test"),
        rlang::dots_list(a = 1L, b = "test"),

        iterations = 1e6
    )

    png("benchmark.png", width = 800, height = 700)
    .benchplot(bm)
    dev.off()

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

        ## DDot Methods
        .init = function(self, a, b) {
            self@a <- a
            self@b <- b
        },

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

    bench::mark(Foo(a = 1L), iterations = 1e6)
    foo <- Foo(a = 1L, b = 2.0)
    foo$to_json_string()
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

    Class("Foo", a = t_int, b = t_char)
    foo <- Foo(a = 1L, b = "test")
    foo@a

    l <- list(a = 1L, b = "2.0")
    Foo(!!!l)

    foos <- purrr::map(list(l, l, l), Foo)

    bench::mark(
        Foo(a = 1L, b = "test"),
        iterations = 1e4
    )

    Enum <- function(.name, ...) {
        .attributes <- rlang::list2(...)

        .self <- .Call(
            "wrap__EnumDefinition__new",
            name = .name,
            values = .attributes,
            PACKAGE = "RS"
        )

        new_enum <- function(...) {
            .Call(
                "wrap__EnumInstance__new",
                values = rlang::list2(...),
                def = .self,
                PACKAGE = "RS"
            )
        }

        assign(.name, new_enum, envir = parent.frame())
    }

    Enum <- function(.name, ...) {
        variants <- rlang::list2(...)
        if (length(variants) == 0) {
            stop("Enum must have at least one variant.", call. = FALSE)
        }

        structure(variants, class = c("Enum", .name))
    }

    # Example usage
    enum <- Enum(
        "Color",
        RED = 1,
        GREEN = 2,
        BLUE = 3
    )

    switch(
        enum$BLUE,

        `1` = "Red",
        `2` = "Green",
        `3` = "Helloooooo Blue!"
    )

    library(RS)

    Class(
        "Asset",

        # FIELDS
        id = private_(t_char),
        company = t_char,
        type = t_char,
        price = t_dbl,
        quantity = t_int,

        # METHODS
        value = function(.self) {
            .self@price * .self@quantity
        },
        print = function(.self) {
            cat("Asset ID:", .self@id, "\n")
            cat("Name:", .self@company, "\n")
            cat("Type:", .self@type, "\n")
            cat("Price:", .self@price, "\n")
            cat("Quantity:", .self@quantity, "\n")
            cat("Total Value:", .self@value(), "\n")
        }
    )

    fields <- list(
        id = "AAPL",
        company = "Apple Inc.",
        type = "Equity",
        price = 150.0,
        quantity = 10L
    )

    asset <- Asset(!!!fields)

    asset@print()

    df <- data.frame(
        id = c("AAPL", "GOOGL", "AMZN"),
        company = c("Apple Inc.", "Alphabet Inc.", "Amazon.com Inc."),
        type = c("Equity", "Equity", "Equity"),
        price = c(150.0, 2800.0, 3400.0),
        quantity = c(10L, 5L, 2L)
    )

    assets <- lapply(seq_len(nrow(df)), \(i) Asset(!!!df[i, ]))

    assets <- purrr::pmap(df, Asset)

    N <- 1e6
    df <- data.frame(
        id = sample(LETTERS, N, replace = TRUE),
        company = sample(letters, N, replace = TRUE),
        type = sample(c("Equity", "Bond", "Commodity"), N, replace = TRUE),
        price = runif(N, 50, 500),
        quantity = sample(1:100, N, replace = TRUE)
    )

    system.time(assets <- purrr::pmap(df, Asset))

    func__ <- function(x) {
        print(x)
    }

    private_(1)

    `:` <- function(x, y) {
        assign(deparse(substitute(x)), y, envir = parent.frame())
    }

    `<<` <- 1

    y <- 1
    x::y

    `:=` <- function(x, y) {
        # if (!inherits(y, "ClassTypeGenerator")) {
        #     stop("`:=` can only be used with ClassTypeGenerator objects.")
        # }
        # assign(deparse(substitute(x)), y, envir = parent.frame())
        nm <- deparse(substitute(x))
        alist(nm = y)
    }

    x := 1

    `:=` <- function(name, value) {
        nm <- deparse(substitute(name))
        setNames(list(value), nm)
    }

    `:=` <- function(x, y) {
        if (!inherits(y, "ClassTypeGenerator")) {
            stop("`:=` can only be used with ClassTypeGenerator objects.")
        }
        pl <- pairlist()
        pl[[deparse(substitute(x))]] <- y
        pl
    }

    f <- function(...) {
        # Flatten any list-of-lists into one flat named list
        args <- list(...)
        # Combine all the small lists into one big flat list
        do.call(c, args)
    }

    f(x := 1, y := 2)
}
