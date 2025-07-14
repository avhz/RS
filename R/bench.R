.benchmark <- function(n) {
    FooRS <- Class(
        "FooRS",
        a := t_int,
        b := t_dbl,
        c := t_char
    )

    FooRSUnval <- Class(
        "FooRSUnval",
        .validate = FALSE,

        a := t_int,
        b := t_dbl,
        c := t_char
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

    FooPy <- reticulate::PyClass(
        "FooPy",
        list(
            a = NULL,
            b = NULL,
            c = NULL,
            `__init__` = function(self, a, b, c) {
                self$a <- a
                self$b <- b
                self$c <- c
                invisible()
            }
        )
    )

    .out <- bench::mark(
        "RS (no validation)" = FooRSUnval(a = 1L, b = 2.0, c = "xxx"),
        "RS" = FooRS(a = 1L, b = 2.0, c = "xxx"),
        "R6" = FooR6$new(a = 1L, b = 2.0, c = "xxx"),
        "S4" = FooS4(a = 1L, b = 2.0, c = "xxx"),
        "S7" = FooS7(a = 1L, b = 2.0, c = "xxx"),
        "reticulate" = FooPy(a = 1L, b = 2.0, c = "xxx"),
        "RefClass" = FooRef(a = 1L, b = 2.0, c = "xxx"),

        iterations = n,
        check = FALSE
    )[, c(1, 3, 4, 5, 6, 8, 9)]

    .out$speedup <- sprintf("%f", .out$`itr/sec`[1] / .out$`itr/sec`)
    return(.out)
}

.benchplot <- function(benchmark) {
    # browser()
    baseline <- benchmark |>
        dplyr::filter(as.character(expression) == "RS (no validation)") |>
        dplyr::pull(`itr/sec`)

    benchmark |>
        dplyr::mutate(
            expr_chr = as.character(expression),
            pkg = factor(expr_chr, levels = expr_chr),
            baseline = benchmark |>
                dplyr::mutate(expr_chr = as.character(expression)) |>
                dplyr::filter(expr_chr == "RS (no validation)") |>
                dplyr::pull(`itr/sec`),
            speedup = baseline / `itr/sec`,
            speedup_label = dplyr::if_else(
                !expr_chr %in% c("RS", "RS (no validation)"),
                paste0(round(speedup, 0), "x"),
                NA_character_
            )
        ) |>
        ggplot2::ggplot() +
        ggplot2::aes(x = pkg, y = `itr/sec`, fill = pkg) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::geom_text(
            ggplot2::aes(label = speedup_label),
            vjust = -0.5,
            na.rm = TRUE,
            size = 6,
        ) +
        ggplot2::ggtitle(
            "CLASS INITIALISATION: ITERATIONS / SECOND (higher is better)"
        ) +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_manual(
            values = c(
                "RS (no validation)" = "#1E2650",
                "RS" = "#1E2650",
                "R6" = "#cacaca",
                "S4" = "#cacaca",
                "S7" = "#cacaca",
                "RefClass" = "#cacaca",
                "reticulate" = "#cacaca"
            )
        ) +
        ggplot2::scale_y_continuous(labels = scales::label_number()) +
        ggplot2::labs(
            x = "Package",
            y = "Iterations per second",
            fill = "Package"
        )
}
