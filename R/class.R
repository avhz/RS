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
    # .method <- function(.m) {
    #     if (is.function(.m) && !inherits(.m, "ClassType")) {
    #         return(structure(.m, class = c("ClassMethod", class(.m))))
    #     }
    #     return(.m)
    # }
    # .methods <- lapply(list(...), .method)
    .attrs <- rlang::list2(...)

    .fields <- lapply(
        .attrs,
        function(.a) if (inherits(attr, "ClassType")) return(.a)
    )

    .self <- .Call(
        "wrap__ClassDefinition__new",
        name = .classname,
        methods = .attrs,
        validate = .validate,
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

    ## Handle un-named arguments
    ## Need to handle 'fields' argument in wrap__ClassInstance__new

    # formals(new_class) <- do.call(
    #     alist,
    #     setNames(rep(list(quote(expr = )), length(.fields)), names(.fields))
    # )

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

    Class("Foo", a = t_int)
    profvis::profvis(foos <- lapply(1:1e5, \(.) Foo(a = 1L)))

    # png("benchmark.png", width = 800, height = 600)
    # .benchplot(bm)
    # dev.off()

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

    ## Define class
    Class(
        "FooParallel",
        a = t_int,

        ## Methods
        bar = function(self, x) self@a + x,
    )

    foo <- FooParallel(a = 1L)
    foo@a
    foo@bar(2L)

    ## Create classes in parallel via Mirai
    mirai::daemons(0L)
    cfg <- mirai::serial_config(
        c("ClassDefinition", "ClassInstance"),
        list(function(x) serialize(x, NULL), function(x) serialize(x, NULL)),
        list(base::unserialize, base::unserialize)
    )

    mirai::daemons(4L, dispatcher = FALSE, serial = cfg)
    e <- mirai::everywhere({
        devtools::load_all()
        Class(
            "FooParallel",
            a = t_int,

            ## Methods
            bar = function(self, x) self@a + x,
        )
    })

    m <- mirai::mirai(
        {
            FooParallel(a = 1L)
        }
        # FooParallel = FooParallel
    )[]

    m
    (foos <- mirai::mirai_map(
        1:3,
        \(i) FooParallel(a = as.integer(i)),
        # FooParallel = FooParallel
    )[])

    foos[1]$data
}
