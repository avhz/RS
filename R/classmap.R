## ============================================================================
## CLASSMAP
## ============================================================================

#' @export
print.ClassMap <- function(x, ...) .print_rust_object(x)

#' @export
.DollarNames.ClassMap <- function(env, pattern = "")
    ls(ClassMap, pattern = pattern)
