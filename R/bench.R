.benchmark <- function(n) {
    Class(
        "FooRS",
        a = t_int,
        b = t_dbl,
        c = t_char
    )

    FooR6 <- R6::R6Class(
        "FooR6",
        public = list(
            a = NULL,
            b = NULL,
            c = NULL,

            initialize = function(a, b, c) {
                self$a <- a
                self$b <- b
                self$c <- c
            }
        )
    )

    FooRef <- setRefClass(
        "FooRef",
        fields = list(a = "integer", b = "numeric", c = "character"),
        where = globalenv()
    )

    FooS7 <- S7::new_class(
        "FooS7",
        properties = list(
            a = S7::class_integer,
            b = S7::class_numeric,
            c = S7::class_character
        )
    )

    FooS4 <- setClass(
        "FooS4",
        slots = list(
            a = "integer",
            b = "numeric",
            c = "character"
        ),
        where = globalenv()
    )

    bench::mark(
        "RS" = FooRS(a = 1L, b = 2.0, c = "xxx"),
        "R6" = FooR6$new(a = 1L, b = 2.0, c = "xxx"),
        "S4" = FooS4(a = 1L, b = 2.0, c = "xxx"),
        "S7" = FooS7(a = 1L, b = 2.0, c = "xxx"),
        "Ref" = FooRef(a = 1L, b = 2.0, c = "xxx"),

        iterations = n,
        check = FALSE
    )
    # print(timings, width = Inf)
    # View(dplyr::mutate(timings, iter_gc = .data$`itr/sec` / .data$n_gc))
}
