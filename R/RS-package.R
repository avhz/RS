#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang !!!
#' @importFrom utils .DollarNames
## usethis namespace: end
NULL

.RS <- list2env(
    list(
        .pkg = "RS",
        .private = "PrivateAttribute",
        .static = "StaticMethod",
        .instance = "ClassInstance",
        .definition = "ClassDefinition",
        .pairlist = "AttributePairlist",
        .typegen = "TypeGenerator"
    ),
    hash = TRUE
)
