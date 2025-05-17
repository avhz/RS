## ============================================================================
## TYPES
## Basically anything with an "is.*" method, plus maybe some other stuff.
## ============================================================================

.new_type <- function(name, validator) {
    force(name)
    force(validator)
    structure(utils::removeSource(validator), class = c("RS_SELF", name))
}

## ANY TYPE
## This is a catch-all type that will match anything.

#' @export
t_any <- .new_type("t_any", \(t) TRUE)

## BASIC TYPES

#' @export
t_int <- .new_type("t_int", \(t) is.integer(t) && length(t) == 1L)
#' @export
t_ints <- .new_type("t_ints", \(t) is.integer(t) && length(t) > 1L)

#' @export
t_dbl <- .new_type("t_dbl", \(t) is.double(t) && length(t) == 1L)
#' @export
t_dbls <- .new_type("t_dbls", \(t) is.double(t) && length(t) > 1L)

#' @export
t_num <- .new_type("t_num", \(t) is.numeric(t) && length(t) == 1L)
#' @export
t_nums <- .new_type("t_nums", \(t) is.numeric(t) && length(t) > 1L)

#' @export
t_char <- .new_type("t_char", \(t) is.character(t) && length(t) == 1L)
#' @export
t_chars <- .new_type("t_chars", \(t) is.character(t) && length(t) > 1L)

#' @export
t_bool <- .new_type("t_bool", \(t) is.logical(t) && length(t) == 1L)
#' @export
t_bools <- .new_type("t_bools", \(t) is.logical(t) && length(t) > 1L)

#' @export
t_cplx <- .new_type("t_cplx", \(t) is.complex(t) && length(t) == 1L)
#' @export
t_cplxs <- .new_type("t_cplxs", \(t) is.complex(t) && length(t) > 1L)

#' @export
t_raw <- .new_type("t_raw", \(t) is.raw(t) && length(t) == 1L)
#' @export
t_raws <- .new_type("t_raws", \(t) is.raw(t) && length(t) > 1L)

#' @export
t_factor <- .new_type("t_factor", \(t) is.factor(t) && length(t) == 1L)
#' @export
t_factors <- .new_type("t_factors", \(t) is.factor(t) && length(t) > 1L)

## COMPOUND TYPES

#' @export
t_list <- .new_type("t_list", \(t) is.list(t))
#' @export
t_array <- .new_type("t_array", \(t) is.array(t))
#' @export
t_vector <- .new_type("t_vector", \(t) is.vector(t))
#' @export
t_matrix <- .new_type("t_matrix", \(t) is.matrix(t))
#' @export
t_dataframe <- .new_type("t_dataframe", \(t) is.data.frame(t))
#' @export
t_hashtab <- .new_type("t_hashtab", \(t) is.hashtab(t))
#' @export
t_environment <- .new_type("t_environment", \(t) is.environment(t))
#' @export
t_pairlist <- .new_type("t_pairlist", \(t) is.pairlist(t))

## EXOTIC TYPES

#' @export
t_func <- .new_type("t_func", \(t) is.function(t))
#' @export
t_expr <- .new_type("t_expr", \(t) is.expression(t))
#' @export
t_call <- .new_type("t_call", \(t) is.call(t))
#' @export
t_sym <- .new_type("t_sym", \(t) is.symbol(t))
#' @export
t_lang <- .new_type("t_lang", \(t) is.language(t))
#' @export
t_obj <- .new_type("t_obj", \(t) is.object(t))
#' @export
t_prim <- .new_type("t_prim", \(t) is.primitive(t))
