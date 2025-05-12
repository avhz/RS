#!/usr/bin/env bash

RPATH=/usr/local/bin/R
PROFILE=profile.out
PROFILER=/opt/homebrew/lib/libprofiler.dylib

DYLD_INSERT_LIBRARIES="$PROFILER" CPUPROFILE="$PROFILE" "$RPATH" --vanilla -f ./profile.R
pprof --text "$RPATH" "$PROFILE" > ./profile.txt

## Put this in profile.R:
# gc()
# remove(list = ls())
# devtools::load_all()

# Class(
#     "Foo",
#     ## Fields
#     a = t_int,
#     b = t_dbl,
#     c = t_char,
#     ## Methods
#     bar = function(.self, x) {
#         cat("Arg 'x' is", x, "\n")
#         cat("Field 'a' is", .self@a, "\n")
#         cat("Field 'b' is", .self@b, "\n")
#         cat("Field 'c' is", .self@c, "\n")
#         cat("Updating field 'c' to 'new value'\n")
#         .self@c <- "new value"
#         print("Calling method 'baz'")
#         .self@baz(1, 2)
#     },
#     ## Static methods
#     baz = function(a, b, c = data.frame(x = 1:5)) {
#         cat("Arg 'a' is", a, "\n")
#         cat("Arg 'b' is", b, "\n")
#         print(c)
#     }
# )
# foos <- lapply(1:1e+5, \(i) Foo(a = i, b = 1.5, c = "xxx"))
