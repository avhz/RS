# ## ============================================================================
# ## TYPES
# ## Basically anything with an "is.*" method, plus maybe some other stuff.
# ## ============================================================================

# .new_type <- function(name, validator) {
#     force(name)
#     force(validator)
#     structure(utils::removeSource(validator), class = c("ClassType", name))
# }

# ## ============================================================================
# ## ANY TYPE
# ## This is a catch-all type that will match anything.
# ## ============================================================================

# #' @export
# t_any <- .new_type("t_any", \(.) TRUE)

# ## ============================================================================
# ## BASIC TYPES
# ## ============================================================================

# #' @export
# t_int <- .new_type("t_int", \(.) is.integer(.) && length(.) == 1L)
# #' @export
# t_ints <- .new_type("t_ints", \(.) is.integer(.) && length(.) > 1L)

# #' @export
# t_dbl <- .new_type("t_dbl", \(.) is.double(.) && length(.) == 1L)
# #' @export
# t_dbls <- .new_type("t_dbls", \(.) is.double(.) && length(.) > 1L)

# #' @export
# t_num <- .new_type("t_num", \(.) is.numeric(.) && length(.) == 1L)
# #' @export
# t_nums <- .new_type("t_nums", \(.) is.numeric(.) && length(.) > 1L)

# #' @export
# t_char <- .new_type("t_char", \(.) is.character(.) && length(.) == 1L)
# #' @export
# t_chars <- .new_type("t_chars", \(.) is.character(.) && length(.) > 1L)

# #' @export
# t_bool <- .new_type("t_bool", \(.) is.logical(.) && length(.) == 1L)
# #' @export
# t_bools <- .new_type("t_bools", \(.) is.logical(.) && length(.) > 1L)

# #' @export
# t_cplx <- .new_type("t_cplx", \(.) is.complex(.) && length(.) == 1L)
# #' @export
# t_cplxs <- .new_type("t_cplxs", \(.) is.complex(.) && length(.) > 1L)

# #' @export
# t_raw <- .new_type("t_raw", \(.) is.raw(.) && length(.) == 1L)
# #' @export
# t_raws <- .new_type("t_raws", \(.) is.raw(.) && length(.) > 1L)

# #' @export
# t_factor <- .new_type("t_factor", \(.) is.factor(.) && length(.) == 1L)
# #' @export
# t_factors <- .new_type("t_factors", \(.) is.factor(.) && length(.) > 1L)

# ## ============================================================================
# ## COMPOUND TYPES
# ## ============================================================================

# #' @export
# t_list <- .new_type("t_list", is.list)
# #' @export
# t_array <- .new_type("t_array", is.array)
# #' @export
# t_vector <- .new_type("t_vector", is.vector)
# #' @export
# t_matrix <- .new_type("t_matrix", is.matrix)
# #' @export
# t_dataframe <- .new_type("t_dataframe", is.data.frame)
# #' @export
# t_hashtab <- .new_type("t_hashtab", is.hashtab)
# #' @export
# t_environment <- .new_type("t_environment", is.environment)
# #' @export
# t_pairlist <- .new_type("t_pairlist", is.pairlist)

# ## ============================================================================
# ## EXOTIC TYPES
# ## ============================================================================

# #' @export
# t_func <- .new_type("t_func", is.function)
# #' @export
# t_expr <- .new_type("t_expr", is.expression)
# #' @export
# t_call <- .new_type("t_call", is.call)
# #' @export
# t_sym <- .new_type("t_sym", is.symbol)
# #' @export
# t_lang <- .new_type("t_lang", is.language)
# #' @export
# t_obj <- .new_type("t_obj", is.object)
# #' @export
# t_prim <- .new_type("t_prim", is.primitive)

# ## ============================================================================
# ## E.O.F.
# ## ============================================================================
