## ============================================================================
## Class
## ============================================================================

#' @title
#' Define a new RS class.
#'
#' @description
#' Create a new ClassInstance in R.
#'
#' @details
#' The Class function creates a new ClassInstance in R.
#' It allows you to define fields and methods for the class.
#'
#' @param .classname The name of the class.
#' @param .validate Whether to validate the class attributes.
#'     Note: setting this to `FALSE` gives a decent performance boost.
#' @param ... The fields and methods of the class definition.
#'
#' @export
Class <- function(.classname, ..., .validate = TRUE) {
    # .method <- function(.m) {
    #     if (is.function(.m) && !inherits(.m, "ClassType")) {
    #         return(structure(.m, class = c("ClassMethod", class(.m))))
    #     }
    #     return(.m)
    # }
    # .methods <- lapply(list(...), .method)

    .self <- .Call(
        "wrap__ClassDefinition__new",
        name = .classname,
        methods = rlang::list2(...),
        validate = .validate,
        PACKAGE = "RS"
    )

    new_class <- function(...) {
        .Call(
            "wrap__ClassInstance__new",
            fields = rlang::list2(...),
            def = .self,
            PACKAGE = "RS"
        )
    }

    assign(.classname, new_class, envir = parent.frame())
}

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

## ============================================================================
## Class utilities
## ============================================================================

#' @export
print.ClassInstance <- function(x, ...) {
    x$print()
    invisible(x)
}

#' @export
.DollarNames.ClassInstance <- function(env, pattern = "") {
    ls(ClassInstance, pattern = pattern)
}

#' @export
`@.ClassInstance` <- function(self, name) {
    .attr <- .Call("wrap__ClassInstance__get", self, name)

    ## FIXME
    # if (inherits(.attr, "ClassPrivateAttribute"))
    #     stop("Attribute is private: ", name, call. = FALSE)

    if (is.function(.attr) && !inherits(.attr, "ClassStaticMethod"))
        return(function(...) .attr(self, ...))

    return(.attr)
}

#' @export
`@<-.ClassInstance` <- function(self, name, value) {
    .Call("wrap__ClassInstance__set", self, name, value)
    return(self)
}

#' @export
print.extendr_error <- function(error, ...) {
    print(error$value)
    invisible(error)
}

#' @export
`==.ClassInstance` <- function(cls1, cls2) {
    if (inherits(cls1, "ClassInstance") && inherits(cls2, "ClassInstance")) {
        return(.Call("wrap__class_equality", cls1, cls2, PACKAGE = "RS"))
    }
    stop("Both arguments must be `ClassInstance` objects.")
}
