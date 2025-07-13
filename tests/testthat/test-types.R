test_that("types.R - t_any", {
    Class("tAny", a := t_any)

    expect_no_error({
        tAny(a = 1L)
        tAny(a = 1.0)
        tAny(a = "x")
        tAny(a = TRUE)
        tAny(a = 1 + 0i)
        tAny(a = as.raw(1))
        tAny(a = factor("x"))
        tAny(a = list(1, 2))
        tAny(a = array(1:10, dim = c(2, 5)))
        tAny(a = matrix(1:10, nrow = 2))
        tAny(a = data.frame(x = 1:5))
        tAny(a = hashtab())
        tAny(a = environment())
        tAny(a = pairlist())
        tAny(a = function(x) x)
        tAny(a = expression(x + 1))
        tAny(a = call("x", 1))
        tAny(a = quote(x))
    })
})

test_that("types.R - t_int", {
    Class("tInt", a := t_int, b := t_ints)
    expect_no_error(tInt(a = 1L, b = c(1L, 2L)))
    expect_error(tInt(a = 1.0, b = c(1.0, 2.0)))
})

test_that("types.R - t_dbl", {
    Class("tDbl", a := t_dbl, b := t_dbls)
    expect_no_error(tDbl(a = 1.0, b = c(1.0, 2.0)))
    expect_error(tDbl(a = 1L, b = c(1L, 2L)))
})

test_that("types.R - t_num", {
    Class("tNum", a := t_num, b := t_nums)
    expect_no_error(tNum(a = 1L, b = c(1L, 2L)))
    expect_no_error(tNum(a = 1.0, b = c(1.0, 2.0)))
    expect_error(tNum(a = "x", b = c("x", "y")))
})

test_that("types.R - t_char", {
    Class("tChar", a := t_char, b := t_chars)
    expect_no_error(tChar(a = "x", b = c("x", "y")))
    expect_error(tChar(a = 1L, b = c(1L, 2L)))
})

test_that("types.R - t_bool", {
    Class("tBool", a := t_bool, b := t_bools)
    expect_no_error(tBool(a = TRUE, b = c(TRUE, FALSE)))
    expect_error(tBool(a = 1L, b = c(1L, 2L)))
})

test_that("types.R - t_cplx", {
    Class("tCplx", a := t_cplx, b := t_cplxs)
    expect_no_error(tCplx(a = 1 + 0i, b = c(1 + 0i, 2 + 0i)))
    expect_error(tCplx(a = 1L, b = c(1L, 2L)))
})

# test_that("types.R - t_raw", {
#     Class("tRaw", a := t_raw, b := t_raws)
#     expect_no_error(tRaw(a = as.raw(1), b = c(as.raw(1), as.raw(2))))
#     expect_error(tRaw(a = 1L, b = c(1L, 2L)))
# })

test_that("types.R - t_factor", {
    Class("tFactor", a := t_factor, b := t_factors)
    expect_no_error(tFactor(a = factor("x"), b = factor(c("x", "y"))))
    expect_error(tFactor(a = 1L, b = c(1L, 2L)))
})

test_that("types.R - t_list", {
    Class("tList", a := t_list)
    expect_no_error(tList(a = list(1, 2)))
    expect_error(tList(a = 1L))
})

test_that("types.R - t_array", {
    Class("tArray", a := t_array)
    expect_no_error(tArray(a = array(1:10, dim = c(2, 5))))
    expect_error(tArray(a = 1L))
})

test_that("types.R - t_vector", {
    Class("tVector", a := t_vector)
    expect_no_error(tVector(a = c(1, 2)))
    expect_error(tVector(a = data.frame()))
})

test_that("types.R - t_matrix", {
    Class("tMatrix", a := t_matrix)
    expect_no_error(tMatrix(a = matrix(1:10, nrow = 2)))
    expect_error(tMatrix(a = 1L))
})

test_that("types.R - t_dataframe", {
    Class("tDataFrame", a := t_dataframe)
    expect_no_error(tDataFrame(a = data.frame(x = 1:5)))
    expect_error(tDataFrame(a = 1L))
})

test_that("types.R - t_hashtab", {
    Class("tHashtab", a := t_hashtab)
    expect_no_error(tHashtab(a = hashtab()))
    expect_error(tHashtab(a = 1L))
})

test_that("types.R - t_environment", {
    Class("tEnvironment", a := t_environment)
    expect_no_error(tEnvironment(a = environment()))
    expect_error(tEnvironment(a = 1L))
})

test_that("types.R - t_pairlist", {
    Class("tPairlist", a := t_pairlist)
    expect_no_error(tPairlist(a = pairlist()))
    expect_error(tPairlist(a = 1L))
})

test_that("types.R - t_func", {
    Class("tFunc", a := t_func)
    expect_no_error(tFunc(a = function(x) x))
    expect_error(tFunc(a = 1L))
})

test_that("types.R - t_expr", {
    Class("tExpr", a := t_expr)
    expect_no_error(tExpr(a = expression(x + 1)))
    expect_error(tExpr(a = 1L))
})

# test_that("types.R - t_call", {
#     Class("tCall", a := t_call)
#     expect_no_error(tCall(a = call("x", 1)))
#     expect_error(tCall(a = 1L))
# })

test_that("types.R - t_sym", {
    Class("tSym", a := t_sym)
    expect_no_error(tSym(a = quote(x)))
    expect_error(tSym(a = 1L))
})

test_that("types.R - t_lang", {
    Class("tLang", a := t_lang)
    lang <- languageEl(function() {}, 1)
    expect_no_error(tLang(a = lang))
    expect_error(tLang(a = 1L))
})

test_that("types.R - t_obj", {
    Class("tObj", a := t_obj)
    obj <- structure(1, class = "foo")
    expect_no_error(tObj(a = obj))
    expect_error(tObj(a = 1L))
})

test_that("types.R - t_prim", {
    Class("tPrim", a := t_prim)
    expect_no_error(tPrim(a = c))
    expect_error(tPrim(a = 1L))
})
