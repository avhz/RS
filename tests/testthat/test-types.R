test_that("types.R", {
    ## All should have one T and one F result

    expect <- c(FALSE, TRUE)

    expect_equal(c(t_any(1L), t_any()), c(T, T))
    expect_equal(c(t_int(1), t_int(1L)), expect)
    expect_equal(c(t_ints(c(1, 2)), t_ints(c(1L, 2L))), expect)
    expect_equal(c(t_dbl(1L), t_dbl(1)), expect)
    expect_equal(c(t_dbls(c(1L, 2L)), t_dbls(c(1, 2))), expect)
    expect_equal(c(t_num("1L"), t_num(1L)), expect)
    expect_equal(c(t_nums(c(1L, "2L")), t_nums(c(1, 2))), expect)
    expect_equal(c(t_char(1L), t_char("x")), expect)
    expect_equal(c(t_chars(c(1L, 2L)), t_chars(c("1", "1"))), expect)
    expect_equal(c(t_bool(1L), t_bool(TRUE)), expect)
    expect_equal(c(t_bools(c(1L, 2L)), t_bools(c(TRUE, TRUE))), expect)
    expect_equal(c(t_cplx(1L), t_cplx(1 + 0i)), expect)
    expect_equal(c(t_cplxs(c(1L, 2L)), t_cplxs(c(1, 1 + 0i))), expect)
    expect_equal(c(t_raw(raw()), t_raw(as.raw(1))), expect)
    expect_equal(c(t_raws(c(1L)), t_raws(raw(2L))), expect)
    expect_equal(c(t_factor(1L), t_factor(factor("x"))), expect)
    expect_equal(c(t_factors(c(1L)), t_factors(factor(c("x", "y")))), expect)
    expect_equal(c(t_list(1L), t_list(list(1))), expect)
    expect_equal(c(t_array(1L), t_array(array(1))), expect)
    expect_equal(c(t_vector(c()), t_vector(c(1))), expect)
    expect_equal(c(t_matrix(1L), t_matrix(matrix(1))), expect)
    expect_equal(c(t_dataframe(1L), t_dataframe(data.frame(x = 1))), expect)
    expect_equal(c(t_hashtab(1L), t_hashtab(hashtab())), expect)
    expect_equal(c(t_environment(1L), t_environment(environment())), expect)
    expect_equal(c(t_pairlist(1L), t_pairlist(pairlist())), expect)
    expect_equal(c(t_func(1L), t_func(c)), expect)
    expect_equal(c(t_expr(1L), t_expr(expression(x + 1))), expect)
    expect_equal(c(t_call(1L), t_call(call("x", 1))), expect)
    expect_equal(c(t_sym(1L), t_sym(quote(x))), expect)
    lang <- languageEl(
        function() {
        },
        1
    )
    expect_equal(c(t_lang(1L), t_lang(lang)), expect)
    obj <- structure(1, class = "foo")
    expect_equal(c(t_obj(1L), t_obj(obj)), expect)
    expect_equal(c(t_prim(1L), t_prim(c)), expect)
})


test_that("types.R - t_any", {
    expect_equal(t_any(1L), TRUE)
    expect_equal(t_any(), TRUE)
    expect_equal(t_any(1:10), TRUE)
    expect_equal(t_any(list()), TRUE)
    expect_equal(t_any(list(1, 2)), TRUE)
    expect_equal(t_any(c(1, 2)), TRUE)
    expect_equal(t_any(c(TRUE, FALSE)), TRUE)
})

test_that("types.R - t_int", {
    expect_equal(rs_class(), "RS_CLASS")
    expect_equal(rs_self(), "RS_SELF")
    expect_equal(rs_method(), "RS_METHOD")
    expect_equal(rs_type(), "RS_TYPE")
    expect_equal(rs_static(), "RS_STATIC")
})
