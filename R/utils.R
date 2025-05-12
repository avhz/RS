## ============================================================================
## UTILS
## ============================================================================

.print_rust_object <- function(.o) {
    if (typeof(.o) == "externalptr") {
        cat(paste(capture.output(str(.o)), format(.o), sep = ""), "\n")
        .o$print()
    }
    stop("Object is not a Rust object (externalptr).")
}

.compile <- function(.f) compiler::cmpfun(.f, options = list(optimize = 3L))
