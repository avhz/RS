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

    (bm <- .benchmark(1e4))
    .benchplot(bm)
    ggplot2::autoplot(bm)
    png("benchmark.png", width = 800, height = 700)
    .benchplot(bm)
    dev.off()
}
