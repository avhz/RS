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

.new_type <- function(name, validator) {
    validator |>
        utils::removeSource() |>
        structure(class = c("ClassType", name))
}

## ============================================================================
## ANY TYPE
## This is a catch-all type that will match anything.
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_any <- .new_type("t_any", \(.) TRUE)

## ============================================================================
## DATE TYPE
## This doesn't have an "is.*" method, but it is a common type.
## ============================================================================

.is_date <- function(.) inherits(., c("Date", "POSIXt"))

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_date <- .new_type("t_date", \(.) .is_date(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dates <- .new_type("t_dates", \(.) .is_date(.) && length(.) > 1L)

## ============================================================================
## BASIC TYPES
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_int <- .new_type("t_int", \(.) is.integer(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_ints <- .new_type("t_ints", \(.) is.integer(.) && length(.) > 1L)

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dbl <- .new_type("t_dbl", \(.) is.double(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dbls <- .new_type("t_dbls", \(.) is.double(.) && length(.) > 1L)

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_num <- .new_type("t_num", \(.) is.numeric(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_nums <- .new_type("t_nums", \(.) is.numeric(.) && length(.) > 1L)

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_char <- .new_type("t_char", \(.) is.character(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_chars <- .new_type("t_chars", \(.) is.character(.) && length(.) > 1L)

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_bool <- .new_type("t_bool", \(.) is.logical(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_bools <- .new_type("t_bools", \(.) is.logical(.) && length(.) > 1L)

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_cplx <- .new_type("t_cplx", \(.) is.complex(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_cplxs <- .new_type("t_cplxs", \(.) is.complex(.) && length(.) > 1L)

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_raw <- .new_type("t_raw", \(.) is.raw(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_raws <- .new_type("t_raws", \(.) is.raw(.) && length(.) > 1L)

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_factor <- .new_type("t_factor", \(.) is.factor(.) && length(.) == 1L)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_factors <- .new_type("t_factors", \(.) is.factor(.) && length(.) > 1L)

## ============================================================================
## COMPOUND TYPES
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_list <- .new_type("t_list", is.list)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_array <- .new_type("t_array", is.array)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_vector <- .new_type("t_vector", \(.) is.vector(.))
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_matrix <- .new_type("t_matrix", is.matrix)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_dataframe <- .new_type("t_dataframe", is.data.frame)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_hashtab <- .new_type("t_hashtab", is.hashtab)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_environment <- .new_type("t_environment", is.environment)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_pairlist <- .new_type("t_pairlist", is.pairlist)

## ============================================================================
## EXOTIC TYPES
## ============================================================================

#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_func <- .new_type("t_func", is.function)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_expr <- .new_type("t_expr", is.expression)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_call <- .new_type("t_call", is.call)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_sym <- .new_type("t_sym", is.symbol)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_lang <- .new_type("t_lang", is.language)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_obj <- .new_type("t_obj", is.object)
#' @export
#' @rdname RSTypes
#' @format NULL
#' @order 1
t_prim <- .new_type("t_prim", is.primitive)

## ============================================================================
## E.O.F.
## ============================================================================
