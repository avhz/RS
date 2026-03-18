Field <- function(
    t,
    default = NULL,
    alias = NULL,
    title = NULL,
    description = NULL,
    exclude = FALSE,
    frozen = FALSE,
    gt = NULL,
    ge = NULL,
    lt = NULL,
    le = NULL,
    min_length = NULL,
    max_length = NULL
) {
    return(.structure(
        t,
        default = default,
        alias = alias,
        title = title,
        description = description,
        exclude = exclude,
        frozen = frozen,
        gt = gt,
        ge = ge,
        lt = lt,
        le = le,
        min_length = min_length,
        max_length = max_length
    ))
}
