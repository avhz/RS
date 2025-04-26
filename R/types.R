## ============================================================================
## TYPES
## ============================================================================

#' Type assignment operator
#' @description
#' `%:%` is a type assignment operator that allows you to assign a type to a variable.
#' @param x The variable to assign the type to.
#' @param y The type to assign to the variable.
#' @return The variable with the assigned type.
#' @export
`%:%` <- function(x, y) if (is.null(y)) x else y(x)

"xxx" %:% NULL # No validation
"xxx" %:% is.character # Validation

#' @name Array type.
#' @title Array type.
#' @description
#' Array type for Class type validation.
#' @export
.array <- is.array

### Types to implement
## Basically anything with an "is.*" method
## is.R
# is.double     == dbl
# is.leaf
# is.name
# is.symbol
# is.array
# is.element
# is.function   == fun
# is.list       == lst
# is.nan
# is.raster
# is.table
# is.atomic
# is.hashtab
# is.loaded
# is.nan.POSIXlt
# is.object
# is.raw        == raw
# is.ts
# is.call
# is.environment == env
# is.logical    == bool
# is.null       == null
# is.ordered
# is.recursive
# is.tskernel
# is.character  == char
# is.expression == expr
# is.matrix     == mat
# is.numeric    == num
# is.relistable
# is.unsorted
# is.complex    == cplx
# is.factor     == fact
# is.integer    == int
# is.mts
# is.numeric.Date
# is.pairlist
# is.single
# is.vector     == vec
# is.data.frame == df
# is.finite
# is.language
# is.na
# is.numeric.POSIXt
# is.primitive
# is.stepfun
