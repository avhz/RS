. <- function() {
    gc()
    remove(list = ls())
    # rextendr::clean()
    rextendr::document()
    devtools::load_all()
    devtools::test()
}
.()

(bm <- .benchmark(1e4))
.benchplot(bm)
ggplot2::autoplot(bm)

# png("benchmark.png", width = 800, height = 600)
# .benchplot(bm)
# dev.off()

Class(
    "Foo",
    a = t_int,
    b = private(t_dbl),

    ## Methods
    bar = private(function(self, x) {
        self@a + x * self@b
    }),
    qux = function(self) {
        print(class(self))
    },

    ## Static methods
    baz = static(function(self, x) {
        self + x
    })
)

bench::mark(Foo(a = 1L), iterations = 1e5)

foo <- Foo(a = 1L, b = 2.0)
foo@qux()
foo@a
foo@b
foo@bar(2L)
foo@baz(2L, 3L)
