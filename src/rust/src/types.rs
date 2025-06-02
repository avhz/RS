#![allow(non_camel_case_types)]

use extendr_api::prelude::*;

extendr_module! {
    mod types;

    fn t_int;
    fn t_ints;
    fn t_dbl;
    fn t_dbls;
    fn t_num;
    fn t_nums;
    fn t_char;
    fn t_chars;
    fn t_bool;
    fn t_bools;
    fn t_cplx;
    fn t_cplxs;
    fn t_raw;
    fn t_raws;
    fn t_factor;
    fn t_factors;
    fn t_list;
    fn t_array;
    fn t_vector;
    fn t_matrix;
    fn t_dataframe;
    fn t_environment;
    fn t_pairlist;
    fn t_func;
    fn t_expr;
    fn t_sym;
    fn t_lang;
    fn t_obj;
    fn t_prim;
}

macro_rules! define_class_type {
    ($name:ident, $type_check:ident, $len_check:expr) => {
        #[inline(always)]
        #[extendr]
        fn $name(x: Robj) -> bool {
            x.$type_check() && ($len_check)(x.len())
        }
    };
}

// ============================================================================
// BASIC TYPES
// ============================================================================

define_class_type!(t_int, is_integer, |len: usize| len == 1);
define_class_type!(t_ints, is_integer, |len: usize| len >= 1);
define_class_type!(t_dbl, is_real, |len: usize| len == 1);
define_class_type!(t_dbls, is_real, |len: usize| len >= 1);
define_class_type!(t_num, is_number, |len: usize| len == 1);
define_class_type!(t_nums, is_number, |len: usize| len >= 1);
define_class_type!(t_char, is_char, |len: usize| len == 1);
define_class_type!(t_chars, is_char, |len: usize| len >= 1);
define_class_type!(t_bool, is_logical, |len: usize| len == 1);
define_class_type!(t_bools, is_logical, |len: usize| len >= 1);
define_class_type!(t_cplx, is_complex, |len: usize| len == 1);
define_class_type!(t_cplxs, is_complex, |len: usize| len >= 1);
define_class_type!(t_raw, is_raw, |len: usize| len == 1);
define_class_type!(t_raws, is_raw, |len: usize| len >= 1);
define_class_type!(t_factor, is_factor, |len: usize| len == 1);
define_class_type!(t_factors, is_factor, |len: usize| len >= 1);

// ============================================================================
// COMPOUND TYPES
// ============================================================================

define_class_type!(t_list, is_list, |_| true);
define_class_type!(t_array, is_array, |_| true);
define_class_type!(t_vector, is_vector, |_| true);
define_class_type!(t_matrix, is_matrix, |_| true);
define_class_type!(t_dataframe, is_frame, |_| true);
define_class_type!(t_environment, is_environment, |_| true);
define_class_type!(t_pairlist, is_pairlist, |_| true);

// ============================================================================
// EXOTIC TYPES
// ============================================================================

define_class_type!(t_func, is_function, |_| true);
define_class_type!(t_expr, is_expressions, |_| true);
define_class_type!(t_sym, is_symbol, |_| true);
define_class_type!(t_lang, is_language, |_| true);
define_class_type!(t_obj, is_object, |_| true);
define_class_type!(t_prim, is_primitive, |_| true);

// ============================================================================
// Need custom implementations for these types
// maybe open PR ?
// ============================================================================

// define_class_type!(t_call, is_call, |_| true);
// define_class_type!(t_hashtab, is_hashtab, |_| true);
// define_class_type!(t_any, is_any, |_| true);
