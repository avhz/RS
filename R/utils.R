## ============================================================================
## UTILS
## ============================================================================

.print_rust_object <- function(.o) {
    .print <- function(.o) {
        cat(paste(capture.output(str(.o)), format(.o), sep = ""), "\n")
        .o$print()
    }

    if (typeof(.o) == "externalptr") {
        .print(.o)
    }

    stop("Object is not a Rust object (externalptr).")
}

.compile <- function(.f) compiler::cmpfun(.f, options = list(optimize = 3L))
