test_that("class.R - Basic Foo class", {
    Class(
        "Foo",

        ## Fields
        a = t_int,
        b = t_dbl,
        c = t_char,

        ## Methods
        bar = function(.self, x) {
            cat("Arg 'x' is", x, "\n")
            cat("Field 'a' is", .self@a, "\n")
            cat("Field 'b' is", .self@b, "\n")
            cat("Field 'c' is", .self@c, "\n")
            cat("Updating field 'c' to 'new value'\n")
            .self@c <- "new value"
        },

        ## Static methods
        baz = function(a, b, c = data.frame(x = 1:5)) {
            cat("Arg 'a' is", a, "\n")
            cat("Arg 'b' is", b, "\n")
            print(c)
        }
    ) |>
        expect_no_error(message = "Class definition should not throw an error")

    expect_no_error({
        foo <- Foo(a = 1L, b = 2.0, c = "xxx")
        foo <- Foo(!!!list(a = 1L, b = 2.0, c = "xxx"))
        foo
        foo@a
        foo@b
        foo@c
        foo@baz(1, 2)
        foo@bar(10)
    }) |>
        capture.output()

    testthat::expect_equal(foo@a, 1L)
    testthat::expect_equal(foo@b, 2.0)

    testthat::expect_equal(foo@bar(10), foo@c) |> capture.output()
    testthat::expect_equal(foo@c, "new value")
})

test_that("class.R - Empty class", {
    expect_no_error({
        Class("Whatever")
        Whatever()
    })
})

test_that("class.R - Basic Asset class", {
    expect_no_error({
        Class(
            "Asset",

            # FIELDS
            id = t_char,
            company = t_char,
            type = t_char,
            price = t_dbl,
            quantity = t_int,

            # METHODS
            value = function(.self) {
                .self@price * .self@quantity
            },
            print = function(.self) {
                cat("Asset ID:", .self@id, "\n")
                cat("Name:", .self@company, "\n")
                cat("Type:", .self@type, "\n")
                cat("Price:", .self@price, "\n")
                cat("Quantity:", .self@quantity, "\n")
                cat("Total Value:", .self@value(), "\n")
            }
        )

        asset <- Asset(
            id = "AAPL",
            company = "Apple Inc.",
            type = "Equity",
            price = 150.0,
            quantity = 10L,
        )

        asset@print() |> capture.output()

        df <- data.frame(
            id = c("AAPL", "GOOGL", "AMZN"),
            company = c("Apple Inc.", "Alphabet Inc.", "Amazon.com Inc."),
            type = c("Equity", "Equity", "Equity"),
            price = c(150.0, 2800.0, 3400.0),
            quantity = c(10L, 5L, 2L)
        )

        ## The !!! doesn't work in testthat ???
        # assets <- lapply(seq_len(nrow(df)), \(i) Asset(!!!df[i, ]))
        # lapply(apply(df, 1, as.list), \(row) Asset(!!!row))

        assets <- lapply(
            seq_len(nrow(df)),
            \(i)
                Asset(
                    id = df[i, "id"],
                    company = df[i, "company"],
                    type = df[i, "type"],
                    price = df[i, "price"],
                    quantity = df[i, "quantity"]
                )
        )
    })
})


test_that("class.R - Black76 pricer class", {
    expect_no_error({
        Class(
            "Black76",

            ## Fields
            F = t_dbl,
            K = t_dbl,
            T = t_dbl,
            r = t_dbl,
            v = t_dbl,

            ## Methods
            call = function(.self) {
                .self@.df() *
                    (pnorm(.self@.d1()) *
                        .self@F -
                        pnorm(.self@.d2()) * .self@K)
            },
            put = function(.self) {
                .self@.df() *
                    (pnorm(-.self@.d2()) *
                        .self@K -
                        pnorm(-.self@.d1()) * .self@F)
            },
            .df = function(.self) {
                exp(-.self@r * .self@T)
            },
            .d1 = function(.self) {
                (log(.self@F / .self@K) + 0.5 * (.self@v)^2 * .self@T) /
                    (.self@v * sqrt(.self@T))
            },
            .d2 = function(.self) {
                (log(.self@F / .self@K) - 0.5 * (.self@v)^2 * .self@T) /
                    (.self@v * sqrt(.self@T))
            }
        )
    })

    testthat::expect_no_error({
        black76 <- Black76(F = 55, K = 100, T = 1, r = 0.05, v = 0.2)
    })

    testthat::expect_equal(black76@call(), 0.005577321615771232282688)
    testthat::expect_equal(black76@put(), 42.81090142414790733483)
})

test_that("class.R - Basic Class composition", {
    testthat::expect_no_error({
        Class("Foo", a = t_int, b = t_dbl, c = t_char)
        Class("Bar", foo = Foo, baz = t_dbl)

        foo <- Foo(a = 1L, b = 2.0, c = "xxx")
        bar <- Bar(foo = foo, baz = 3.0)
    })
})
