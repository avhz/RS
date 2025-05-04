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

#[extendr]
fn __new_class__(name: &str, definition_args: List, instance_args: List, methods: Strings) -> List {
    let mut definition_map = ClassMap::from_list(definition_args);

    methods.into_iter().for_each(|key| {
        // Get the method from definition_map
        let method = definition_map.get(key.to_string());

        if let Some(method_fn) = method.as_function() {
            if let (Some(body), Some(formals), Some(environment)) = (
                method_fn.body().and_then(|b| b.as_language()),
                method_fn.formals(),
                method_fn.environment(),
            ) {
                let filtered_formals: Vec<_> =
                    formals.iter().filter(|(k, _)| *k != ".self").collect();

                let new_formals = Pairlist::from_pairs(filtered_formals);

                // let self_pairs = vec![("map", definition_map.clone().into())];
                let mut _self = List::from_pairs([("map", definition_map.clone().into())]);
                _self.set_class(&[name, "Self".into()]).unwrap();

                let env = Environment::new_with_parent(environment);
                env.set_local(Symbol::from_string(".self"), _self);

                if let Ok(new_method) = Function::from_parts(new_formals, body, env) {
                    definition_map.set(key.to_string(), new_method.as_robj().clone());
                }
            }
        }
    });

    let instance_map = instance_args.into_hashmap();

    for key in instance_map.keys().cloned().collect::<Vec<_>>() {
        // I guess this is ok since the key is already in the map? i.e. for key in keys
        let post = instance_map.get(key).expect("ERROR: Key not found");
        let pre = definition_map.get(key.into());

        if let Some(mut post_class) = post.class() {
            if post_class.any(|c| c == "Class") {
                definition_map.set(key.into(), post.into());
                continue;
            }
        }

        if let Some(validator_fn) = pre.as_function() {
            let check = validator_fn
                .call(pairlist!(post))
                .expect("ERROR: Validator function failed for attribute: {key}")
                .as_bool()
                .unwrap_or(false);

            if check {
                definition_map.set(key.into(), post.into());
                continue;
            }

            let msg = format!(
                "Invalid type <'{}'> for field <'{}'>.",
                post.as_str().unwrap_or("<unknown>"), // WE NEED typeof(x) HERE !
                key
            );
            panic!("{}", msg);
        }
    }

    let mut object = List::from_pairs(vec![("map", definition_map.into())]);
    object.set_class(&[name, "Class".into()]).unwrap();
    object
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    // Module name.
    mod RS;

    // ClassMap implementation.
    impl ClassMap;

    // __new_class__ implementation.
    fn __new_class__;
}
