// ============================================================================
// IMPORTS
// ============================================================================

use extendr_api::prelude::*;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// ============================================================================
// EXTENDR MODULE
// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
// ============================================================================

extendr_module! {
    mod RS;

    impl ClassDefinition;
    impl ClassInstance;
    impl Type;

    fn private_;
    fn is_private;
    fn static_;
    fn is_static;
    fn structure_;
}

// ============================================================================
// GLOBALS
// ============================================================================

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

type RobjMap = HashMap<&'static str, Robj>;

// ============================================================================
// STRUCTS
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
struct RcMap(Rc<RobjMap>);

#[derive(Debug, Clone, PartialEq)]
struct RcRefMap(Rc<RefCell<RobjMap>>);

#[derive(Debug, Clone, PartialEq)]
struct RcClassDefinition(Rc<ClassDefinition>);

#[extendr]
#[derive(Debug, Clone, PartialEq)]
struct ClassDefinition {
    /// The name of the class.
    name: Strings,

    /// The methods shared by the class instances.
    methods: RcMap,

    /// Whether to validate the class instance fields.
    validate: bool,
}

#[extendr]
#[derive(Debug, PartialEq)]
struct ClassInstance {
    /// The individual instance data.
    fields: RcRefMap,

    /// Pointer to the class definition this instance belongs to.
    definition: RcClassDefinition,
}

// ============================================================================
// IMPLEMENTATIONS
// ============================================================================

impl RcMap {
    fn from_hashmap(map: RobjMap) -> Self {
        Self(Rc::new(map))
    }

    fn from_list(list: List) -> Self {
        Self::from_hashmap(list.into_hashmap())
    }
}

impl RcRefMap {
    fn from_hashmap(map: RobjMap) -> Self {
        Self(Rc::new(RefCell::new(map)))
    }
}

#[extendr]
impl ClassDefinition {
    fn new(name: Strings, methods: List, validate: bool) -> Robj {
        let class_definition = Rc::new(Self {
            name,
            methods: RcMap::from_list(methods),
            validate,
        });

        ExternalPtr::new(RcClassDefinition(class_definition)).into()
    }
}

#[extendr]
impl ClassInstance {
    #[inline(always)]
    fn new(fields: List, def: Robj) -> Result<Self> {
        let map = fields.into_hashmap();

        // Need to check that all fields are named.
        // i.e. NOT a list like: list(x = 1, 2, 3)
        // Seems that emtpy keys are "NA" despite docs saying they are "".
        // See:
        // https://extendr.github.io/extendr/extendr_api/wrapper/list/struct.List.html#method.into_hashmap

        if map.keys().any(|k| *k == "NA") {
            return Err(Error::Other("ClassInstance fields must be named.".into()));
        }

        let def_ptr: ExternalPtr<RcClassDefinition> = def.try_into()?;
        let def_ref = RcClassDefinition(def_ptr.0.clone());
        let def_map = &def_ref.0.methods.0;

        // Closure to compare classes for a given key
        // i.e. check the class of the passed value is the
        // same as the expected class in the class definition.
        let both_inherit = |key: &str, class: &str| {
            // Get the value in the definition map
            let mut c1 = def_map
                .get(key)
                .and_then(|v| v.class())
                .unwrap_or(StrIter::default());

            // Get the value in the instance map
            let mut c2 = map
                .get(key)
                .and_then(|v| v.class())
                .unwrap_or(StrIter::default());

            // Compare the classes (RstrIter's)
            // This is basically the same as: c.inherits(class)
            c1.any(|c| c == class) && c2.any(|c| c == class)
        };

        if def_ref.0.validate {
            for (key, value) in &map {
                // Class composition
                if both_inherit(key, "ClassInstance") {
                    continue;
                }

                if let Some(expected) = def_map
                    .get(key)
                    .and_then(|v| <ExternalPtr<Type>>::try_from(v.clone()).ok().map(|p| *p))
                {
                    if !validate(&value, &expected)? {
                        let msg = format!(
                            "Invalid type <'{}'> passed for field <'{}'>.",
                            call!("typeof", value)?.as_str().unwrap_or("unknown"),
                            key
                        );
                        return Err(Error::Other(msg.into()));
                    }
                }
            }
        }

        Ok(Self {
            fields: RcRefMap::from_hashmap(map),
            definition: def_ref,
        })
    }

    fn print(&self) {
        let name = &self.definition.0.name;
        let fields = &self.fields.0.borrow();
        let methods = &self.definition.0.methods.0;

        rprintln!("ClassInstance {:?} @ {:p} {{", name, &self);
        for (key, value) in fields.iter() {
            rprintln!("    {}: {:?}", key, value);
        }
        for (key, value) in methods.iter() {
            if value.inherits("Type") {
                continue;
            }
            let value_str = format!("{:?}", value);
            let trimmed_value = if value_str.starts_with("function") {
                if let Some(end) = value_str.find(')') {
                    format!("{}", &value_str[..=end]) //, " [method]")
                } else {
                    format!("{}", value_str)
                }
            } else {
                format!("{:?}", value)
            };
            rprintln!("    {}: {}", key, trimmed_value);
        }
        rprintln!("}}");
    }

    fn name(&self) -> Strings {
        self.definition.0.name.clone().into()
    }

    fn get(&self, key: &str) -> Result<Robj> {
        // Private attributes start with a "."
        // Need to be careful here:
        //  - Should still be accessible from within the class itself
        if key.starts_with('.') {
            let msg = format!("Attribute '{}' is private, or does not exist.", key);
            return Err(Error::Other(msg.into()));
        }

        let definition = &self.definition.0.methods.0;
        let instance = &self.fields.0.borrow();

        // 1. Check instance fields first
        if let Some(field) = instance.get(key) {
            return Ok(field.clone());
        }

        // 2. Check methods in the class definition
        if let Some(method) = definition.get(key) {
            return Ok(method.clone());
        }

        // 3. If not found
        let msg = format!(
            "Attribute '{}' not found in class instance '{:?}'",
            key, self.definition.0.name
        );
        Err(Error::Other(msg.into()))
    }

    fn set(&mut self, key: &'static str, value: Robj) -> Result<Robj> {
        let fields = &self.fields;
        let methods = &self.definition.0.methods.0;

        // Support composition of classes
        if (&value).inherits("ClassInstance") {
            if let Some(value) = fields.0.borrow_mut().insert(key, value.clone()) {
                return Ok(value);
            }
        }

        if let Some(expected) = methods
            .get(key)
            .and_then(|v| <ExternalPtr<Type>>::try_from(v.clone()).ok().map(|p| *p))
        {
            if validate(&value, &expected)? {
                let inserted = fields.0.borrow_mut().insert(key, value.clone());
                if let Some(value) = inserted {
                    return Ok(value);
                }
            }

            let msg = format!(
                "Invalid type <'{}'> passed for field <'{}'>.",
                call!("typeof", value)?.as_str().unwrap_or("unknown"),
                key
            );
            return Err(Error::Other(msg.into()));
        }

        let msg = format!(
            "Unable to set attribute '{:?}' with key '{}' in class '{:?}'",
            &value, key, self.definition.0.name
        );
        Err(Error::Other(msg.into()))
    }

    // fn to_json_string(&self) -> Result<String> {
    //     use serde_json;
    //     let fields = self.fields.0.borrow().clone();
    //     serde_json::to_string_pretty(&fields).map_err(|e| Error::Other(e.to_string().into()))
    // }
}

// ============================================================================
// TYPES
// ============================================================================

#[allow(non_camel_case_types)]
#[extendr]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
    // CATCH-ALL
    t_any,

    // DATES
    t_date,
    t_dates,

    // BASIC TYPES (atomic vectors)
    t_int,
    t_ints,
    t_dbl,
    t_dbls,
    t_num,
    t_nums,
    t_char,
    t_chars,
    t_bool,
    t_bools,
    t_cplx,
    t_cplxs,
    t_raw,
    t_raws,
    t_factor,
    t_factors,

    // COMPOUND TYPES
    t_list,
    t_array,
    t_vector,
    t_matrix,
    t_dataframe,
    t_hashtab,
    t_environment,
    t_pairlist,

    // EXOTIC TYPES
    t_func,
    t_expr,
    t_call,
    t_sym,
    t_lang,
    t_obj,
    t_prim,
}

#[extendr]
impl Type {
    fn print(&self) {
        rprintln!("Type: {:?} @ {:p}", &self, &self);
    }

    fn from_str(s: &str) -> Self {
        Self::from(s)
    }

    fn as_str(&self) -> &'static str {
        Self::into(*self)
    }
}

fn is_date(robj: &Robj) -> bool {
    robj.inherits(&"Date") || robj.inherits(&"POSIXt")
}

// #[extendr]
fn validate(
    // The Robj is the value passed into the instance field.
    robj: &Robj,
    // The expected class type (contained in the class definition).
    expected_type: &Type,
) -> Result<bool> {
    let is_scalar = robj.len() == 1;

    match expected_type {
        // CATCH-ALL
        Type::t_any => Ok(true),

        // DATES
        Type::t_date => Ok(is_date(&robj) && is_scalar),
        Type::t_dates => Ok(is_date(&robj) && !is_scalar),

        // BASIC TYPES (atomic vectors)
        Type::t_int => Ok(robj.is_integer() && is_scalar),
        Type::t_ints => Ok(robj.is_integer() && !is_scalar),
        Type::t_dbl => Ok(robj.is_real() && is_scalar),
        Type::t_dbls => Ok(robj.is_real() && !is_scalar),
        Type::t_num => Ok(robj.is_number() && is_scalar),
        Type::t_nums => Ok(robj.is_number() && !is_scalar),
        Type::t_char => Ok(robj.is_string() && is_scalar),
        Type::t_chars => Ok(robj.is_string() && !is_scalar),
        Type::t_bool => Ok(robj.is_logical() && is_scalar),
        Type::t_bools => Ok(robj.is_logical() && !is_scalar),
        Type::t_cplx => Ok(robj.is_complex() && is_scalar),
        Type::t_cplxs => Ok(robj.is_complex() && !is_scalar),
        Type::t_raw => Ok(robj.inherits("raw") && is_scalar),
        Type::t_raws => Ok(robj.inherits("raw") && !is_scalar),
        Type::t_factor => Ok(robj.is_factor() && is_scalar),
        Type::t_factors => Ok(robj.is_factor() && !is_scalar),

        // COMPOUND TYPES
        Type::t_array => Ok(robj.is_array()),
        Type::t_list => Ok(robj.is_list()),
        Type::t_vector => Ok(robj.is_vector_atomic()),
        Type::t_matrix => Ok(robj.is_matrix()),
        Type::t_dataframe => Ok(robj.is_frame()),
        Type::t_hashtab => Ok(robj.inherits(&"hashtab")),
        Type::t_environment => Ok(robj.is_environment()),
        Type::t_pairlist => Ok(robj.is_pairlist()),

        // EXOTIC TYPES
        Type::t_func => Ok(robj.is_function()),
        Type::t_expr => Ok(robj.is_expressions()),
        Type::t_call => Ok(robj.inherits(&"call")),
        Type::t_sym => Ok(robj.is_symbol()),
        Type::t_lang => Ok(robj.is_language()),
        Type::t_obj => Ok(robj.is_object()),
        Type::t_prim => Ok(robj.is_primitive()),
    }
}

impl From<&str> for Type {
    fn from(s: &str) -> Self {
        match s {
            "t_any" => Self::t_any,
            "t_date" => Self::t_date,
            "t_dates" => Self::t_dates,
            "t_int" => Self::t_int,
            "t_ints" => Self::t_ints,
            "t_dbl" => Self::t_dbl,
            "t_dbls" => Self::t_dbls,
            "t_num" => Self::t_num,
            "t_nums" => Self::t_nums,
            "t_char" => Self::t_char,
            "t_chars" => Self::t_chars,
            "t_bool" => Self::t_bool,
            "t_bools" => Self::t_bools,
            "t_cplx" => Self::t_cplx,
            "t_cplxs" => Self::t_cplxs,
            "t_raw" => Self::t_raw,
            "t_raws" => Self::t_raws,
            "t_factor" => Self::t_factor,
            "t_factors" => Self::t_factors,
            "t_list" => Self::t_list,
            "t_array" => Self::t_array,
            "t_vector" => Self::t_vector,
            "t_matrix" => Self::t_matrix,
            "t_dataframe" => Self::t_dataframe,
            "t_hashtab" => Self::t_hashtab,
            "t_environment" => Self::t_environment,
            "t_pairlist" => Self::t_pairlist,
            "t_func" => Self::t_func,
            "t_expr" => Self::t_expr,
            "t_call" => Self::t_call,
            "t_sym" => Self::t_sym,
            "t_lang" => Self::t_lang,
            "t_obj" => Self::t_obj,
            "t_prim" => Self::t_prim,
            _ => Self::t_any,
        }
    }
}

impl Into<&'static str> for Type {
    fn into(self) -> &'static str {
        match self {
            Type::t_any => "t_any",
            Type::t_date => "t_date",
            Type::t_dates => "t_dates",
            Type::t_int => "t_int",
            Type::t_ints => "t_ints",
            Type::t_dbl => "t_dbl",
            Type::t_dbls => "t_dbls",
            Type::t_num => "t_num",
            Type::t_nums => "t_nums",
            Type::t_char => "t_char",
            Type::t_chars => "t_chars",
            Type::t_bool => "t_bool",
            Type::t_bools => "t_bools",
            Type::t_cplx => "t_cplx",
            Type::t_cplxs => "t_cplxs",
            Type::t_raw => "t_raw",
            Type::t_raws => "t_raws",
            Type::t_factor => "t_factor",
            Type::t_factors => "t_factors",
            Type::t_list => "t_list",
            Type::t_array => "t_array",
            Type::t_vector => "t_vector",
            Type::t_matrix => "t_matrix",
            Type::t_dataframe => "t_dataframe",
            Type::t_hashtab => "t_hashtab",
            Type::t_environment => "t_environment",
            Type::t_pairlist => "t_pairlist",
            Type::t_func => "t_func",
            Type::t_expr => "t_expr",
            Type::t_call => "t_call",
            Type::t_sym => "t_sym",
            Type::t_lang => "t_lang",
            Type::t_obj => "t_obj",
            Type::t_prim => "t_prim",
        }
    }
}

// ============================================================================
// DECORATORS
// ============================================================================

/// @title
/// Decorator to mark a class attribute as private.
///
/// @description
/// Declare a method or attribute as private.
///
/// The `private` function is used to declare a method or attribute as private
/// in a class definition.
///
/// Private methods and attributes are not accessible from outside the class,
/// and are used to encapsulate data that should not be modified directly.
///
/// @param attribute The function or attribute to be declared as private.
///
/// @export
#[extendr]
fn private_(attribute: Robj) -> Result<Robj> {
    structure_(
        attribute,
        Strings::from_values(["PrivateAttribute"]),
        List::default(),
    )
}

/// Check if an attribute is private.
#[extendr]
fn is_private(attribute: Robj) -> bool {
    attribute.inherits("PrivateAttribute")
}

/// @title
/// Decorator to mark a method as static.
///
/// @description
/// The `static_` decorator is used to declare a method
/// as a static method in a class definition.
/// Static methods do not refer to `self`,
/// i.e. the first argument is not the instance of the class.
///
/// A static method is a method that belongs to the class itself,
/// rather than to instances of the class.
/// TO-DO: It can be called without creating an instance of the class.
///
/// @param attribute The function to be declared as a static method.
///
/// @export
#[extendr]
pub fn static_(attribute: Robj) -> Result<Robj> {
    structure_(
        attribute,
        Strings::from_values(["StaticMethod"]),
        List::default(),
    )
}

/// Check if a method is static.
#[extendr]
fn is_static(attribute: Robj) -> bool {
    attribute.inherits("StaticMethod")
}

/// Function to structure an R object with a class and attributes.
///
/// @keywords internal
#[extendr]
fn structure_(robj: Robj, class: Strings, attribs: List) -> Result<Robj> {
    let mut obj = robj.clone();

    // Set classes
    let mut class_vec: Vec<&str> = obj.class().into_iter().flatten().collect();
    class_vec.extend(class.iter().map(Rstr::as_str));
    class_vec.push("RS"); // Ensure "RS" is always included
    obj.set_attrib(class_symbol(), &class_vec)?;

    // Set attributes
    for (name, value) in attribs.iter() {
        obj.set_attrib(name, value)?;
    }

    Ok(obj)
}
