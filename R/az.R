## ============================================================================
## CLASS UTILITIES
## ============================================================================

#' @export
print.ClassInstance <- function(x, ...) {
    cat(.Call(wrap__ClassInstance__print, x))
}

#' @export
.DollarNames.ClassInstance <- function(x, pattern = "") {
    ls(ClassInstance, pattern = pattern)
}

#' @export
`@.ClassInstance` <- function(self, name) {
    .attr <- .Call(wrap__ClassInstance__get, self, name)

    ## FIXME
    if (inherits(.attr, .RS[[".private"]])) {
        stop("Attribute is private: ", name, call. = FALSE)
    }

    if (is.function(.attr) && !inherits(.attr, .RS[[".static"]])) {
        return(function(...) .attr(self, ...))
    }

    return(.attr)
}

#' @export
`@<-.ClassInstance` <- function(self, name, value) {
    .Call(wrap__ClassInstance__set, self, name, value)
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
    .classes <- c(.class, class(.x), "RS")
    structure(.x, class = .classes, ...)
}
