## ============================================================================
## Decorators
## These are used to decorate methods and attributes in a class definition.
## ============================================================================

#' @title
#' Declare a method is a static method.
#'
#' @description
#' Declare a method is a static method.
#'
#' @details
#' The `static` function is used to declare a method as a static method
#' in a class definition.
#' Static methods do not refer to `self`,
#' i.e. the first argument is not the instance of the class.
#'
#' @param .attr The function to be declared as a static method.
#'
#' @export
static <- function(.attr) {
    ## TODO: Improve this....
    if (!is.function(.attr)) {
        stop("`static` must be called on a function.")
    }
    structure(
        .attr,
        class = c("ClassStaticMethod", class(.attr))
    )
}

#' @title
#' Declare a method or attribute as private.
#'
#' @description
#' Declare a method or attribute as private.
#'
#' @details
#' The `private` function is used to declare a method or attribute as private
#' in a class definition.
#' Private methods and attributes are not accessible from outside the class.
#'
#' @param .attr The function or attribute to be declared as private.
#'
#' @export
private <- function(.attr) {
    structure(
        .attr,
        class = c("ClassPrivateAttribute", class(.attr))
    )
}
