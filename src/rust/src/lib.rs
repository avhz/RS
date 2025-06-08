// ============================================================================
// IMPORTS
// ============================================================================

use extendr_api::{prelude::*, robj};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
// use ahash::AHashMap as HashMap;
// use std::{cell::RefCell, rc::Rc};

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
}

// ============================================================================
// GLOBALS
// ============================================================================

// #[global_allocator]
// static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

type RobjMap = HashMap<String, Robj>;

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

fn list_to_robjmap(list: List) -> RobjMap {
    list.into_hashmap()
        .into_iter()
        .map(|(k, v)| (k.into(), v))
        .collect()
}

impl RcMap {
    fn from_hashmap(map: RobjMap) -> Self {
        Self(Rc::new(map))
    }

    fn from_list(list: List) -> Self {
        Self::from_hashmap(list_to_robjmap(list))
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
    fn new(fields: List, def: Robj) -> Result<Self> {
        let def_ptr: ExternalPtr<RcClassDefinition> = def.try_into()?;
        let def_ref = RcClassDefinition(def_ptr.0.clone());

        let map = list_to_robjmap(fields);

        if def_ref.0.validate {
            let def_map = def_ref.0.methods.0.clone();

            // Only collect validator functions once
            // let validators: HashMap<&String, Function> = def_map
            //     .iter()
            //     .filter_map(|(k, v)| v.as_function().map(|f| (k, f)))
            //     .collect();

            for (key, value) in &map {
                // Support composition of classes
                if value.inherits("ClassInstance") {
                    continue;
                }

                // Only check validators if method exists
                if let Some(validator) = def_map.get(key).and_then(|v| v.as_function()) {
                    if !validator.call(pairlist!(value))?.as_bool().unwrap_or(false) {
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
        let methods = self.definition.0.methods.0.clone();

        rprintln!("ClassInstance {:?} @ {:p} {{", name, &self);
        rprintln!("    fields:  {:?}", fields);
        rprintln!("    methods: {:?}", methods.keys());
        rprintln!("}}");
    }

    fn name(&self) -> Strings {
        self.definition.0.name.clone().into()
    }

    fn get(&self, key: &str) -> Result<Robj> {
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

    fn set(&mut self, key: String, value: Robj) -> Result<Robj> {
        let fields = &self.fields;
        let methods = &self.definition.0.methods.0.clone();

        // Support composition of classes
        if value.inherits("ClassInstance") {
            let inserted = fields.0.borrow_mut().insert(key.to_string(), value.clone());
            if let Some(value) = inserted {
                return Ok(value);
            }
        }

        if let Some(validator) = methods.clone().get(&key).and_then(|v| v.as_function()) {
            let arg = Pairlist::from_pairs([("", value.clone())]);

            if validator.call(arg)?.as_bool().unwrap_or(false) {
                let inserted = fields.0.borrow_mut().insert(key.to_string(), value.clone());
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
}

// ============================================================================
// TYPES
// ============================================================================

#[allow(non_camel_case_types)]
#[extendr]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClassType {
    t_any,
    t_date,
    t_dates,
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
    t_list,
    t_array,
    t_vector,
    t_matrix,
    t_dataframe,
    t_hashtab,
    t_environment,
    t_pairlist,
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

#[extendr]
impl ClassType {
    fn print(&self) {
        rprintln!("ClassType: {:?} @ {:p}", &self, &self);
    }

    fn infer(robj: Robj) -> Self {
        let is_scalar = robj.len() == 1;

        match robj {
            // BASIC TYPES (atomic vectors)
            _ if robj.is_integer() && is_scalar => ClassType::t_int,
            _ if robj.is_integer() && !is_scalar => ClassType::t_ints,
            _ if robj.is_real() && is_scalar => ClassType::t_dbl,
            _ if robj.is_real() && !is_scalar => ClassType::t_dbls,
            _ if robj.is_number() && is_scalar => ClassType::t_num,
            _ if robj.is_number() && !is_scalar => ClassType::t_nums,
            _ if robj.is_char() && is_scalar => ClassType::t_char,
            _ if robj.is_char() && !is_scalar => ClassType::t_chars,
            _ if robj.is_logical() && is_scalar => ClassType::t_bool,
            _ if robj.is_logical() && !is_scalar => ClassType::t_bools,
            _ if robj.is_complex() && is_scalar => ClassType::t_cplx,
            _ if robj.is_complex() && !is_scalar => ClassType::t_cplxs,
            _ if robj.is_raw() && is_scalar => ClassType::t_raw,
            _ if robj.is_raw() && !is_scalar => ClassType::t_raws,
            _ if robj.is_factor() && is_scalar => ClassType::t_factor,
            _ if robj.is_factor() && !is_scalar => ClassType::t_factors,
            // DATES
            _ if is_date(&robj) && is_scalar => ClassType::t_date,
            _ if is_date(&robj) && !is_scalar => ClassType::t_dates,
            // COMPOUND TYPES
            _ if robj.inherits(&"list") => ClassType::t_list,
            _ if robj.is_array() => ClassType::t_array,
            _ if robj.is_vector_atomic() => ClassType::t_vector,
            _ if robj.is_matrix() => ClassType::t_matrix,
            _ if robj.inherits(&"data.frame") => ClassType::t_dataframe,
            _ if robj.is_pairlist() => ClassType::t_pairlist,
            _ if robj.inherits(&"hashtab") => ClassType::t_hashtab,
            _ if robj.is_environment() => ClassType::t_environment,
            // EXOTIC TYPES
            _ if robj.is_function() => ClassType::t_func,
            _ if robj.is_expressions() => ClassType::t_expr,
            _ if robj.is_symbol() => ClassType::t_sym,
            _ if robj.is_language() => ClassType::t_lang,
            _ if robj.is_primitive() => ClassType::t_prim,
            _ if robj.is_object() => ClassType::t_obj,
            _ if robj.inherits(&"call") => ClassType::t_call,
            // FALLBACK
            _ => ClassType::t_any,
        }
    }

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "t_any" => Ok(ClassType::t_any),
            "t_date" => Ok(ClassType::t_date),
            "t_dates" => Ok(ClassType::t_dates),
            "t_int" => Ok(ClassType::t_int),
            "t_ints" => Ok(ClassType::t_ints),
            "t_dbl" => Ok(ClassType::t_dbl),
            "t_dbls" => Ok(ClassType::t_dbls),
            "t_num" => Ok(ClassType::t_num),
            "t_nums" => Ok(ClassType::t_nums),
            "t_char" => Ok(ClassType::t_char),
            "t_chars" => Ok(ClassType::t_chars),
            "t_bool" => Ok(ClassType::t_bool),
            "t_bools" => Ok(ClassType::t_bools),
            "t_cplx" => Ok(ClassType::t_cplx),
            "t_cplxs" => Ok(ClassType::t_cplxs),
            "t_raw" => Ok(ClassType::t_raw),
            "t_raws" => Ok(ClassType::t_raws),
            "t_factor" => Ok(ClassType::t_factor),
            "t_factors" => Ok(ClassType::t_factors),
            "t_list" => Ok(ClassType::t_list),
            "t_array" => Ok(ClassType::t_array),
            "t_vector" => Ok(ClassType::t_vector),
            "t_matrix" => Ok(ClassType::t_matrix),
            "t_dataframe" => Ok(ClassType::t_dataframe),
            "t_hashtab" => Ok(ClassType::t_hashtab),
            "t_environment" => Ok(ClassType::t_environment),
            "t_pairlist" => Ok(ClassType::t_pairlist),
            "t_func" => Ok(ClassType::t_func),
            "t_expr" => Ok(ClassType::t_expr),
            "t_call" => Ok(ClassType::t_call),
            "t_sym" => Ok(ClassType::t_sym),
            "t_lang" => Ok(ClassType::t_lang),
            "t_obj" => Ok(ClassType::t_obj),
            "t_prim" => Ok(ClassType::t_prim),
            _ => Err(Error::Other(format!("Unknown ClassType: {}", s).into())),
        }
    }
}
