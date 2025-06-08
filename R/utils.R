## ============================================================================
## Class utilities
## ============================================================================

#' @export
print.ClassInstance <- function(x, ...) {
    print(.Call("wrap__ClassInstance__print", x))
    # out <- capture.output(.Call("wrap__ClassInstance__print", x))
    # cat(out, sep = "\n")
    invisible(x)
}

#' @export
.DollarNames.ClassInstance <- function(x, pattern = "") {
    ls(ClassInstance, pattern = pattern)
}

#' @export
`@.ClassInstance` <- function(self, name) {
    .attr <- .Call("wrap__ClassInstance__get", self, name)

    ## FIXME
    # if (inherits(.attr, "ClassPrivateAttribute"))
    #     stop("Attribute is private: ", name, call. = FALSE)

    if (is.function(.attr) && !inherits(.attr, "ClassStaticMethod")) {
        return(function(...) .attr(self, ...))
    }

    return(.attr)
}

#' @export
`@<-.ClassInstance` <- function(self, name, value) {
    .Call("wrap__ClassInstance__set", self, name, value)
    return(self)
}

#' @export
print.extendr_error <- function(x, ...) {
    print(x$value)
    invisible(x)
}

#' @export
`==.ClassInstance` <- function(cls1, cls2) {
    if (inherits(cls1, "ClassInstance") && inherits(cls2, "ClassInstance")) {
        return(.Call("wrap__class_equality", cls1, cls2, PACKAGE = "RS"))
    }
    stop("Both arguments must be `ClassInstance` objects.")
}


## ============================================================================
## GENERAL UTILS
## ============================================================================

.print_rust_object <- function(.o) {
    if (typeof(.o) != "externalptr") {
        stop("Object is not a Rust object (externalptr).")
    }
    cat(
        paste(utils::capture.output(utils::str(.o)), format(.o), sep = ""),
        "\n"
    )
    .o$print()
}

.compile <- function(.f) {
    compiler::cmpfun(.f, options = list(optimize = 3L))
}
