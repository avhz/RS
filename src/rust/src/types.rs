#![allow(non_camel_case_types)]

use extendr_api::prelude::*;

extendr_module! {
    mod types;
    impl t_int;
    impl t_ints;
    impl t_dbl;
    impl t_dbls;
    impl t_num;
    impl t_nums;
    impl t_char;
    impl t_chars;
    impl t_bool;
    impl t_bools;
    impl t_cplx;
    impl t_cplxs;
    impl t_raw;
    impl t_raws;
    impl t_factor;
    impl t_factors;
    impl t_list;
    impl t_array;
    impl t_vector;
    impl t_matrix;
    impl t_dataframe;
    impl t_environment;
    impl t_pairlist;
    impl t_func;
    impl t_expr;
    impl t_sym;
    impl t_lang;
    impl t_obj;
    impl t_prim;
}

#[extendr]
trait ClassType {
    fn validate(&self) -> bool;
}

#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_int;

#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_ints;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_dbl;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_dbls;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_num;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_nums;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_char;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_chars;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_bool;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_bools;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_cplx;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_cplxs;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_raw;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_raws;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_factor;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_factors;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_list;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_array;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_vector;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_matrix;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_dataframe;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_environment;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_pairlist;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_func;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_expr;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_sym;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_lang;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_obj;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_prim;
#[extendr]
#[derive(Debug, Clone, Copy)]
struct t_call;

macro_rules! define_class_type {
    ($name:ident, $type_check:ident, $len_check:expr) => {
        // #[extendr]
        // #[derive(Debug, Clone, Copy)]
        // struct $name;

        #[extendr]
        impl ClassType for $name {
            fn validate(&self) -> bool {
                let x = self.into_robj();
                x.$type_check() && ($len_check)(x.len())
            }
        }
    };
}

// macro_rules! define_class_type {
//     ($name:ident, $type_check:ident, $len_check:expr) => {
//         #[extendr]
//         #[derive(Debug, Clone, Copy)]
//         pub struct $name;

//         #[extendr]
//         impl $name {
//             pub fn validate(&self) -> bool {
//                 let x = self.into_robj();
//                 x.$type_check() && ($len_check)(x.len())
//             }
//         }
//     };
// }

// #[extendr]
// impl ClassType for t_int {
//     fn validate(&self) -> bool {
//         let x = self.into_robj();
//         x.is_integer() && x.len() == 1
//     }
// }

// #[extendr]
// fn validate_type(x: Robj) -> bool {
//     x.validate()
// }

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
