test_that("types.R", {
    ## TYPES
    ## We need to define some types that can be validated, like Pydantic roughly

    # .type <- function(t, f) {}

    # foo <- function(...) {
    #     args <- rlang::list2(...)
    #     for (a in names(args)) {
    #         print(args[[a]])
    #     }
    # }

    # foo(!!!list(a = 1L, b = 2.0, c = "xxx"))


    # cls_i64 <- function(x) is.integer(x) || stop("x must be integer")
    # cls_f64 <- function(x) is.double(x) || stop("x must be double")
    # cls_str <- function(x) is.character(x) || stop("x must be character")

    # test_types <- function() {
    #     cls_i64(1L) == TRUE
    #     cls_i64(1.0)
    #     cls_f64(1.0) == TRUE
    #     cls_f64("a") == FALSE
    #     cls_str("a") == TRUE
    #     cls_str(1) == FALSE
    # }
    # test_types()
})
