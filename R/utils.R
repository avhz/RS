## ============================================================================
## UTILS
## ============================================================================

.print_rust_object <- function(.o) {
    if (typeof(.o) != "externalptr") {
        stop("Object is not a Rust object (externalptr).")
    }
    cat(paste(capture.output(str(.o)), format(.o), sep = ""), "\n")
    .o$print()
}

.compile <- function(.f) {
    compiler::cmpfun(.f, options = list(optimize = 3L))
}

## SPECIAL CLASS NAMES
RS_CLASS <- .Call("wrap__rs_class", PACKAGE = "RS")
RS_SELF <- .Call("wrap__rs_self", PACKAGE = "RS")
RS_METHOD <- .Call("wrap__rs_method", PACKAGE = "RS")
RS_TYPE <- .Call("wrap__rs_type", PACKAGE = "RS")
RS_STATIC <- .Call("wrap__rs_static", PACKAGE = "RS")
