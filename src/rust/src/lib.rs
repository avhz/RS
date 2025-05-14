// ============================================================================
// IMPORTS
// ============================================================================

#![allow(non_camel_case_types)]

use extendr_api::{pairlist, prelude::*};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// ============================================================================
// CLASSMAP
// ============================================================================

#[extendr]
#[derive(Debug)]
struct ClassMap {
    data: Rc<RefCell<HashMap<String, Robj>>>,
}

#[extendr]
impl ClassMap {
    /// Initialize a new Class.
    ///
    /// @export
    fn init(_name: String, _definition_args: List, _instance_args: List, _methods: List) {
        todo!("Implement Class::init");
    }

    /// Create a new Class from an R List.
    ///
    /// @export
    fn from_list(list: List) -> Self {
        let data = list
            .into_hashmap()
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect();

        Self {
            data: Rc::new(RefCell::new(data)),
        }
    }

    /// Set a value in the Class.
    ///
    /// @export
    fn set(&mut self, key: String, value: Robj) {
        self.data.borrow_mut().insert(key, value);
    }

    /// Set multiple values in the Class.
    ///
    /// @export
    fn set_multiple(&mut self, values: List) {
        for (key, value) in values.into_hashmap() {
            self.data.borrow_mut().insert(key.to_string(), value);
        }
    }

    /// Get a value from the Class.
    ///
    /// @export
    fn get(&self, key: String) -> Robj {
        self.data
            .borrow()
            .get(&key)
            .unwrap_or(&"ERROR: Key not found".into())
            .clone()
    }

    /// Get the keys of the Class.
    ///
    /// @export
    fn keys(&self) -> Vec<String> {
        self.data.borrow().keys().cloned().collect()
    }

    /// Get the values of the Class.
    ///
    /// @export
    fn values(&self) -> Vec<Robj> {
        self.data.borrow().values().cloned().collect()
    }

    /// Remove a key-value pair from the Class.
    ///
    /// @export
    fn remove(&mut self, key: String) -> Robj {
        self.data
            .borrow_mut()
            .remove(&key)
            .unwrap_or("ERROR: Key not found".into())
    }

    /// Print the Class.
    ///
    /// @export
    fn print(&self) {
        println!("{:?}", self.data.borrow());
    }

    /// Retain only the elements specified by the predicate.
    ///
    /// @export
    fn retain(&mut self, f: Robj) {
        if !f.is_function() {
            panic!("ERROR: Expected a function. Got: {:?}", f);
        }

        self.data.borrow_mut().retain(|key, value| {
            f.as_function()
                .unwrap()
                .call(pairlist!(key.clone(), value.clone()))
                .unwrap_or(Robj::from(NA_LOGICAL))
                .as_bool()
                .unwrap_or(false)
        });
    }

    /// Clear the Class.
    ///
    /// Clears the map, removing all key-value pairs.
    /// Keeps the allocated memory for reuse.
    ///
    /// @export
    fn clear(&mut self) {
        self.data.borrow_mut().clear();
    }

    /// Check for the existence of a key in the Class.
    ///
    /// @export
    fn contains_key(&self, key: String) -> bool {
        self.data.borrow().contains_key(&key)
    }

    /// Check for the existence of a key in the Class.
    ///
    /// Note: This is an alias for `contains_key`.
    ///
    /// @export
    fn has(&self, key: String) -> bool {
        self.data.borrow().contains_key(&key)
    }

    /// Clone the Class.
    ///
    /// @export
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
        }
    }
}

impl TryFrom<Robj> for ClassMap {
    type Error = Error;

    fn try_from(value: Robj) -> Result<Self> {
        let result: Result<ExternalPtr<Self>> = value.try_into();

        if let Ok(ptr) = result {
            Ok(ClassMap {
                data: ptr.data.clone(),
            })
        } else {
            Err(Error::TypeMismatch("Expected ClassMap instance.".into()))
        }
    }
}

impl From<List> for ClassMap {
    fn from(list: List) -> Self {
        Self::from_list(list)
    }
}

// #[extendr]
// fn set_methods(name: &str, methods: List) -> ClassMap {
//     let mut methods: ClassMap = methods.into();

//     let mut _self = List::from_pairs([("map", methods.clone().into())]);
//     _self.set_class([name, "Self"]).unwrap();

//     for key in methods.iter() {
//         let method = definition_map.get(key.to_string());

//         if let Some(method_fn) = method.as_function() {
//             if let (Some(body), Some(formals), Some(environment)) = (
//                 method_fn.body().and_then(|b| b.as_language()),
//                 method_fn.formals(),
//                 method_fn.environment(),
//             ) {
//                 let new_formals = Pairlist::from_pairs(
//                     &formals
//                         .iter()
//                         .filter(|(k, _)| *k != ".self") // Exclude .self from formals
//                         .collect::<Vec<_>>(),
//                 );

//                 environment.set_local(Symbol::from_string(".self"), &_self);

//                 if let Ok(new_method) = Function::from_parts(new_formals, body, environment) {
//                     definition_map.set(key.to_string(), new_method.into());
//                 }
//             }
//         }
//     }
// }

#[extendr]
fn define_class(name: &str, definition_args: List, methods: Strings) -> Result<List> {
    let mut definition_map = ClassMap::from_list(definition_args);

    let mut _self = List::from_pairs([("map", definition_map.clone().into())]);
    _self.set_class([name, "Self"]).unwrap();

    for key in methods.iter() {
        let method = definition_map.get(key.to_string());

        if let Some(method_fn) = method.as_function() {
            if let (Some(body), Some(formals), Some(environment)) = (
                method_fn.body().and_then(|b| b.as_language()),
                method_fn.formals(),
                method_fn.environment(),
            ) {
                let new_formals = Pairlist::from_pairs(
                    &formals
                        .iter()
                        .filter(|(k, _)| *k != ".self") // Exclude .self from formals
                        .collect::<Vec<_>>(),
                );

                environment.set_local(Symbol::from_string(".self"), &_self);

                if let Ok(new_method) = Function::from_parts(new_formals, body, environment) {
                    definition_map.set(key.to_string(), new_method.into());
                }
            }
        }
    }

    Ok(_self)
}

#[extendr]
fn new_class2(
    name: &str,
    validate: bool,
    self_: List,
    instance_args: List,
    // definition_args: List,
    // methods: Strings,
) -> Result<List> {
    let _self = self_.into_hashmap();
    let mut definition_map = ClassMap::try_from(_self.get("map").unwrap().clone())?;

    for (key, value) in instance_args.into_hashmap() {
        let after = value;

        // Allows for composition of classes.
        if let Some(class) = after.class() {
            if class.into_iter().any(|c| c == "Class") {
                definition_map.set(key.into(), after.into());
                continue;
            }
        }

        if validate {
            let before = definition_map.get(key.into());
            if let Some(validator_fn) = before.as_function() {
                let check = validator_fn
                    .call(pairlist!(after.clone()))
                    .expect("ERROR: Validator function failed for attribute: {key}")
                    .as_bool()
                    .unwrap_or(false);

                if check {
                    definition_map.set(key.into(), after.into());
                    continue;
                }

                let msg = format!(
                    "Invalid type <'{:?}'> passed for field <'{}'>.",
                    after.rtype(),
                    key
                );
                return Err(Error::TypeMismatch(msg.into()));
            }
        } else {
            definition_map.set(key.into(), after.into());
        }
    }

    let mut _self = List::from_pairs([("map", definition_map.clone().into())]);
    _self.set_class([name, "Class"]).unwrap();
    Ok(_self)
}

#[extendr]
fn new_class(
    name: &str,
    validate: bool,
    definition_args: List,
    instance_args: List,
    methods: Strings,
) -> Result<List> {
    let mut definition_map = ClassMap::from_list(definition_args);

    let mut _self = List::from_pairs([("map", definition_map.clone().into())]);
    _self.set_class([name, "Self"]).unwrap();

    for key in methods.iter() {
        let method = definition_map.get(key.to_string());

        if let Some(method_fn) = method.as_function() {
            if let (Some(body), Some(formals), Some(environment)) = (
                method_fn.body().and_then(|b| b.as_language()),
                method_fn.formals(),
                method_fn.environment(),
            ) {
                let new_formals = Pairlist::from_pairs(
                    &formals
                        .iter()
                        .filter(|(k, _)| *k != ".self") // Exclude .self from formals
                        .collect::<Vec<_>>(),
                );

                environment.set_local(Symbol::from_string(".self"), &_self);

                if let Ok(new_method) = Function::from_parts(new_formals, body, environment) {
                    definition_map.set(key.to_string(), new_method.into());
                }
            }
        }
    }

    for (key, value) in instance_args.into_hashmap() {
        let after = value;

        // Allows for composition of classes.
        if let Some(class) = after.class() {
            if class.into_iter().any(|c| c == "Class") {
                definition_map.set(key.into(), after.into());
                continue;
            }
        }

        if validate {
            let before = definition_map.get(key.into());
            if let Some(validator_fn) = before.as_function() {
                let check = validator_fn
                    .call(pairlist!(after.clone()))
                    .expect("ERROR: Validator function failed for attribute: {key}")
                    .as_bool()
                    .unwrap_or(false);

                if check {
                    definition_map.set(key.into(), after.into());
                    continue;
                }

                let msg = format!(
                    "Invalid type <'{:?}'> passed for field <'{}'>.",
                    after.rtype(),
                    key
                );
                return Err(Error::TypeMismatch(msg.into()));
            }
        } else {
            definition_map.set(key.into(), after.into());
        }
    }

    _self.set_class([name, "Class"]).unwrap();
    Ok(_self)
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    // Module name.
    mod RS;

    impl ClassMap;
    fn new_class;
    fn new_class2;
    fn define_class;
}
