## ============================================================================
## UTILS
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
