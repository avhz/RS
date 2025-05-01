use extendr_api::prelude::*;

pub(crate) trait Type {
    fn validate(&self) -> bool;
    fn class(&self) -> String;
}

pub(crate) struct t_int(pub i32);
pub(crate) struct t_ints(pub Vec<i32>);
pub(crate) struct t_dbl(pub f64);
pub(crate) struct t_dbls(pub Vec<f64>);

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    // Module name.
    mod RS;

    // // ClassMap implementation.
    // impl ClassMap;

    // // __new_class__ implementation.
    // fn __new_class__;
}
