## ============================================================================
## CLASS UTILITIES
## ============================================================================

#' @title
#' Declare a new class attribute.
#'
#' @description
#' Declare a new class attribute using the `:=` operator.
#'
#' @param lhs The name of the attribute.
#' @param rhs The type of the attribute.
#'
#' @export
`:=` <- function(lhs, rhs) {
    .valid <- c(.RS[[".definition"]], .RS[[".typegen"]], "function", "RS")

    if (!any(class(rhs) %in% .valid)) {
        stop("`:=` can only be used with RS types.")
    }

    pl <- pairlist()
    pl[[deparse(substitute(lhs))]] <- rhs
    structure_(pl, .RS[[".pairlist"]], list())
}


#' @export
print.ClassInstance <- function(x, ...) {
    cat(.Call(wrap__ClassInstance__print, x, PACKAGE = "RS"))
}

#' @export
.DollarNames.ClassInstance <- function(x, pattern = "") {
    ls(ClassInstance, pattern = pattern)
}

#' @export
`@.ClassInstance` <- function(self, name) {
    .attr <- .Call(wrap__ClassInstance__get, self, name, PACKAGE = "RS")
    ## TODO: Move to Rust ClassInstance.get()
    if (is.function(.attr) && !inherits(.attr, .RS[[".static"]])) {
        return(function(...) .attr(self, ...))
    }
    return(.attr)
}


#' @export
`@<-.ClassInstance` <- function(self, name, value) {
    .Call(wrap__ClassInstance__set, self, name, value, PACKAGE = "RS")
    return(self)
}

#' @export
print.extendr_error <- function(x, ...) {
    print(x$value)
    invisible(x)
}


## ============================================================================
## GENERAL UTILS
## ============================================================================

.print_rust_object <- function(.o) {
    if (typeof(.o) != "externalptr") {
        stop("Object is not a Rust object (externalptr).")
    }
    .str <- utils::capture.output(utils::str(.o))
    .fmt <- format(.o)
    cat(paste(.str, .fmt, sep = ""), "\n")
}

.compile <- function(.f) {
    compiler::cmpfun(.f, options = list(optimize = 3L))
}

.structure <- function(.x, .class, ...) {
    structure(.x, class = c(.class, class(.x), "RS"), ...)
}
