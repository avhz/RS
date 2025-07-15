#' RS types for validation of RS class attributes.
#'
#' @description
#' Supported types for RS classes include:
#'
#' * `t_any`
#' * `t_date`, `t_dates`
#' * `t_int`, `t_ints`
#' * `t_dbl`, `t_dbls`
#' * `t_num`, `t_nums`
#' * `t_char`, `t_chars`
#' * `t_bool`, `t_bools`
#' * `t_cplx`, `t_cplxs`
#' * `t_raw`, `t_raws`
#' * `t_factor`, `t_factors`
#' * `t_list`
#' * `t_array`
#' * `t_vector`
#' * `t_matrix`
#' * `t_dataframe`
#' * `t_hashtab`
#' * `t_environment`
#' * `t_pairlist`
#' * `t_func`
#' * `t_expr`
#' * `t_call`
#' * `t_sym`
#' * `t_lang`
#' * `t_obj`
#' * `t_prim`
#'
#' Note that there are scalar and vector versions of some types,
#' such as `t_int` and `t_ints`.
#' The former allows for a single scalar integer,
#' while the latter matches a vector of integers.
#'
#' There is also the catch-all type `t_any`,
#' which will match any value, and is useful for cases where
#' you want to allow any type of value for a field,
#' while still validating the other fields,
#' i.e. when `.validation = TRUE` (the default).
#'
#' @order 0
#' @name RSTypes
#' @return A new type object that can be used for validation in RS classes.
#' @examples
#'
#' t_any
#' t_date
#' t_ints
#' t_dataframe
NULL

## ============================================================================
## TYPES
## Basically anything with an "is.*" method, plus maybe some other stuff.
## ============================================================================

.new_type_generator <- function(name) {
    .structure(
        function() .Call(wrap__ClassType__from_str, name, PACKAGE = "RS"),
        .RS[[".typegen"]]
    )
}

#' @export
print.ClassType <- function(x, ...) {
    invisible(.Call(wrap__ClassType__print, x, PACKAGE = "RS"))
}

## ============================================================================
## ANY TYPE
## This is a catch-all type that will match anything.
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_any <- .new_type_generator("t_any")

## ============================================================================
## DATE TYPE
## This doesn't have an "is.*" method, but it is a common type.
## ============================================================================

.is_date <- function(.) inherits(., c("Date", "POSIXt"))

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_date <- .new_type_generator("t_date")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dates <- .new_type_generator("t_dates")

## ============================================================================
## BASIC TYPES
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_int <- .new_type_generator("t_int")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_ints <- .new_type_generator("t_ints")

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dbl <- .new_type_generator("t_dbl")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dbls <- .new_type_generator("t_dbls")

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_num <- .new_type_generator("t_num")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_nums <- .new_type_generator("t_nums")

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_char <- .new_type_generator("t_char")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_chars <- .new_type_generator("t_chars")

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_bool <- .new_type_generator("t_bool")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_bools <- .new_type_generator("t_bools")

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_cplx <- .new_type_generator("t_cplx")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_cplxs <- .new_type_generator("t_cplxs")

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_raw <- .new_type_generator("t_raw")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_raws <- .new_type_generator("t_raws")

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_factor <- .new_type_generator("t_factor")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_factors <- .new_type_generator("t_factors")

## ============================================================================
## COMPOUND TYPES
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_list <- .new_type_generator("t_list")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_array <- .new_type_generator("t_array")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_vector <- .new_type_generator("t_vector")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_matrix <- .new_type_generator("t_matrix")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dataframe <- .new_type_generator("t_dataframe")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_hashtab <- .new_type_generator("t_hashtab")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_environment <- .new_type_generator("t_environment")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_pairlist <- .new_type_generator("t_pairlist")

## ============================================================================
## EXOTIC TYPES
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_func <- .new_type_generator("t_func")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_expr <- .new_type_generator("t_expr")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_call <- .new_type_generator("t_call")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_sym <- .new_type_generator("t_sym")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_lang <- .new_type_generator("t_lang")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_obj <- .new_type_generator("t_obj")
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_prim <- .new_type_generator("t_prim")

## ============================================================================
## E.O.F.
## ============================================================================
