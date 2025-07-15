// ============================================================================
// IMPORTS
// ============================================================================

use extendr_api::prelude::*;
use serde_json;
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
    impl ClassType;

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
        let def_ptr: ExternalPtr<RcClassDefinition> = def.try_into()?;
        let def_ref = RcClassDefinition(def_ptr.0.clone());

        let map = fields.into_hashmap();

        let def_map = &def_ref.0.methods.0;

        if def_ref.0.validate {
            for (key, value) in &map {
                // Support composition of classes
                if (&value).inherits("ClassInstance") {
                    continue;
                }

                if let Some(expected) = def_map.get(key).and_then(|v| {
                    <ExternalPtr<ClassType>>::try_from(v.clone())
                        .ok()
                        .map(|p| *p)
                }) {
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
            if value.inherits("ClassType") {
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
        // self.fields
        //     .0
        //     .borrow()
        //     .get(key)
        //     .cloned()
        //     .or_else(|| self.definition.0.methods.0.get(key).cloned())
        //     .ok_or_else(|| Error::Other(format!("Key '{}' not found", key).into()));

        // 1. Check instance fields
        if let Some(value) = self.fields.0.borrow().get(key) {
            return Ok(value.clone());
        }

        // 2. Check methods in the class definition
        if let Some(method) = (&self.definition.0).methods.0.clone().get(key) {
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

        if let Some(expected) = methods.get(key).and_then(|v| {
            <ExternalPtr<ClassType>>::try_from(v.clone())
                .ok()
                .map(|p| *p)
        }) {
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

    fn to_json_string(&self) -> Result<String> {
        let fields = self.fields.0.borrow().clone();
        serde_json::to_string_pretty(&fields).map_err(|e| Error::Other(e.to_string().into()))
    }
}

// ============================================================================
// TYPES
// ============================================================================

#[allow(non_camel_case_types)]
#[extendr]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
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

fn is_date(robj: &Robj) -> bool {
    robj.inherits(&"Date") || robj.inherits(&"POSIXt")
}

// #[extendr]
fn validate(
    // The Robj is the value passed into the instance field.
    robj: &Robj,
    // The expected class type (contained in the class definition).
    expected_type: &ClassType,
) -> Result<bool> {
    let is_scalar = robj.len() == 1;

    match expected_type {
        // CATCH-ALL
        ClassType::t_any => Ok(true),

        // DATES
        ClassType::t_date => Ok(is_date(&robj) && is_scalar),
        ClassType::t_dates => Ok(is_date(&robj) && !is_scalar),

        // BASIC TYPES (atomic vectors)
        ClassType::t_int => Ok(robj.is_integer() && is_scalar),
        ClassType::t_ints => Ok(robj.is_integer() && !is_scalar),
        ClassType::t_dbl => Ok(robj.is_real() && is_scalar),
        ClassType::t_dbls => Ok(robj.is_real() && !is_scalar),
        ClassType::t_num => Ok(robj.is_number() && is_scalar),
        ClassType::t_nums => Ok(robj.is_number() && !is_scalar),
        ClassType::t_char => Ok(robj.is_string() && is_scalar),
        ClassType::t_chars => Ok(robj.is_string() && !is_scalar),
        ClassType::t_bool => Ok(robj.is_logical() && is_scalar),
        ClassType::t_bools => Ok(robj.is_logical() && !is_scalar),
        ClassType::t_cplx => Ok(robj.is_complex() && is_scalar),
        ClassType::t_cplxs => Ok(robj.is_complex() && !is_scalar),
        ClassType::t_raw => Ok(robj.inherits("raw") && is_scalar),
        ClassType::t_raws => Ok(robj.inherits("raw") && !is_scalar),
        ClassType::t_factor => Ok(robj.is_factor() && is_scalar),
        ClassType::t_factors => Ok(robj.is_factor() && !is_scalar),

        // COMPOUND TYPES
        ClassType::t_array => Ok(robj.is_array()),
        ClassType::t_list => Ok(robj.is_list()),
        ClassType::t_vector => Ok(robj.is_vector_atomic()),
        ClassType::t_matrix => Ok(robj.is_matrix()),
        ClassType::t_dataframe => Ok(robj.is_frame()),
        ClassType::t_hashtab => Ok(robj.inherits(&"hashtab")),
        ClassType::t_environment => Ok(robj.is_environment()),
        ClassType::t_pairlist => Ok(robj.is_pairlist()),

        // EXOTIC TYPES
        ClassType::t_func => Ok(robj.is_function()),
        ClassType::t_expr => Ok(robj.is_expressions()),
        ClassType::t_call => Ok(robj.inherits(&"call")),
        ClassType::t_sym => Ok(robj.is_symbol()),
        ClassType::t_lang => Ok(robj.is_language()),
        ClassType::t_obj => Ok(robj.is_object()),
        ClassType::t_prim => Ok(robj.is_primitive()),
    }
}

#[extendr]
impl ClassType {
    fn print(&self) {
        rprintln!("ClassType: {:?} @ {:p}", &self, &self);
    }

    fn from_str(s: &str) -> Self {
        Self::from(s)
    }
}

impl From<&str> for ClassType {
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

// ============================================================================
// DECORATORS
// ============================================================================

/// Decorator to mark a class attribute as private.
///
/// A private attribute is not accessible from outside the class.
/// It is used to encapsulate data that should not be modified directly.
#[extendr]
fn private_(attribute: Robj) -> Result<Robj> {
    let mut attr = attribute.clone();
    attr.set_class(["ClassPrivateAttribute"])?;
    Ok(attr)
}

/// Check if an attribute is private.
#[extendr]
fn is_private(attribute: Robj) -> bool {
    attribute.inherits("ClassPrivateAttribute")
}

/// Decorator to mark a method as static.
///
/// A static method is a method that belongs to the class itself,
/// rather than to instances of the class.
/// It can be called without creating an instance of the class,
/// and it does not have access to instance-specific data (self).
#[extendr]
fn static_(attribute: Robj) -> Result<Robj> {
    let mut attr = attribute.clone();
    attr.set_class(["StaticAttribute"])?;
    Ok(attr)
}

/// Check if a method is static.
#[extendr]
fn is_static(attribute: Robj) -> bool {
    attribute.inherits("StaticAttribute")
}

/// Function to structure an R object with a class and attributes.
#[extendr]
fn structure_(robj: Robj, class: Strings, attribs: List) -> Result<Robj> {
    let mut obj = robj.clone();

    // Set classes
    let mut class_vec: Vec<&str> = obj.class().into_iter().flatten().collect();
    class_vec.extend(class.iter().map(Rstr::as_str));
    class_vec.push("RS"); // Ensure "RS" is always included
    obj.set_class(&class_vec)?;

    // Set attributes
    for (name, value) in attribs.iter() {
        obj.set_attrib(name, value)?;
    }

    Ok(obj)
}
