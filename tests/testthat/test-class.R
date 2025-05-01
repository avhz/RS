test_that("class.R", {
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
        foo@c
    }) |>
        capture.output()

    expect_no_error({
        Class("Whatever")
        Whatever()
    })

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
