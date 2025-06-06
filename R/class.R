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
Class <- function(.classname, ..., .validate = TRUE) {
    # .method <- function(.m) {
    #     if (is.function(.m) && !inherits(.m, "ClassType")) {
    #         return(structure(.m, class = c("ClassMethod", class(.m))))
    #     }
    #     return(.m)
    # }

    # .methods <- lapply(list(...), .method)

    .self <- .Call(
        "wrap__ClassDefinition__new",
        name = .classname,
        methods = list(...),
        # methods = .methods,
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

    assign(.classname, new_class, envir = parent.frame())
}

#' @export
print.ClassInstance <- function(x, ...) {
    x$print()
    invisible(x)
}

#' @export
.DollarNames.ClassInstance <- function(env, pattern = "") {
    ls(ClassInstance, pattern = pattern)
}

#' @export
`@.ClassInstance` <- function(self, name) {
    .attr <- .Call("wrap__ClassInstance__get", self, name)

    ## FIXME
    # if (inherits(.attr, "ClassPrivateAttribute"))
    #     stop("Attribute is private: ", name, call. = FALSE)

    if (is.function(.attr) && !inherits(.attr, "ClassStaticMethod"))
        return(function(...) .attr(self, ...))

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
    invisible(error)
}

#' @export
`==.ClassInstance` <- function(cls1, cls2) {
    if (inherits(cls1, "ClassInstance") && inherits(cls2, "ClassInstance")) {
        return(.Call("wrap__class_equality", cls1, cls2, PACKAGE = "RS"))
    }
    stop("Both arguments must be `ClassInstance` objects.")
}

#' @export
static <- function(.attr) {
    ## TODO: Improve this....
    if (!is.function(.attr)) {
        stop("`static` must be called on a function.")
    }
    structure(
        .attr,
        class = c("ClassStaticMethod", class(.attr))
    )
}

#' @export
private <- function(.attr) {
    structure(
        .attr,
        class = c("ClassPrivateAttribute", class(.attr))
    )
}


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

    foo <- Foo(a = 1L)
    foo@qux()
    foo@a
    foo@b
    foo@bar(2L)
    foo@baz(2L, 3L)
}
