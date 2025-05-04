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
struct Classy {
    _name: String,
    _self: List,
    definition: ClassMap,
    instance: List,
    methods: Strings,
}

#[extendr]
impl Classy {
    /// Create a new Class.
    fn new__(name: &str, definition: List, instance: List, methods: Strings) -> Self {
        Self {
            _name: name.to_string(),
            _self: List::new(definition.len()),
            definition: ClassMap::from_list(definition),
            instance,
            methods,
        }
    }

    /// Return class name.
    fn get_name(&self) -> String {
        self._name.clone()
    }

    /// Return self.
    fn get_self(&self) -> List {
        self._self.clone()
    }

    /// Initialize the Class.
    fn init__(&mut self) -> &mut Self {
        self._self = List::from_pairs([("map", self.definition.clone().into())]);
        self._self
            .set_class(&[self._name.clone(), "Self".into()])
            .unwrap();

        self.methods.iter().for_each(|key| {
            // Get the method from definition_map
            let method = self.definition.get(key.to_string());

            if let Some(method_fn) = method.as_function() {
                if let (Some(body), Some(formals), Some(environment)) = (
                    method_fn.body().and_then(|b| b.as_language()),
                    method_fn.formals(),
                    method_fn.environment(),
                ) {
                    let filtered_formals: Vec<_> =
                        formals.iter().filter(|(k, _)| *k != ".self").collect();
                    let new_formals = Pairlist::from_pairs(filtered_formals);
                    let env = Environment::new_with_parent(environment);
                    env.set_local(Symbol::from_string(".self"), self._self.clone());
                    if let Ok(new_method) = Function::from_parts(new_formals, body, env) {
                        self.definition
                            .set(key.to_string(), new_method.as_robj().clone());
                    }
                }
            }
        });

        for (key, value) in self
            .instance
            .clone()
            .into_hashmap()
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
        {
            // I guess this is ok since the key is already in the map? i.e. for key in keys
            let pre = self.definition.get(key.clone());

            if let Some(post_class) = value.class() {
                let check = post_class.into_iter().any(|c| c == "Class");

                if check {
                    self.definition.set(key.into(), value.clone());
                    continue;
                }
            }

            if let Some(validator_fn) = pre.as_function() {
                let check = validator_fn
                    .call(pairlist!(value.clone()))
                    .expect("ERROR: Validator function failed for attribute: {key}")
                    .as_bool()
                    .unwrap_or(false);

                if check {
                    self.definition.set(key.into(), value.clone().into());
                    continue;
                }

                let msg = format!(
                    "Invalid type <'{}'> for field <'{}'>.",
                    value.clone().as_str().unwrap_or("<unknown>"), // WE NEED typeof(x) HERE !
                    key
                );
                panic!("{}", msg);
            }
        }

        // let mut object = List::from_pairs([("map", self.definition.clone().into())]);
        // object
        //     .set_class([self._name.clone(), "Class".into()])
        //     .unwrap();
        // object

        self._self = List::from_pairs([("map", self.definition.clone().into())]);
        self._self
            .set_class([self._name.clone(), "Class".into()])
            .unwrap();

        self
    }
}

#[extendr]
#[derive(Debug)]
struct ClassMap {
    data: Rc<RefCell<HashMap<String, Robj>>>,
}

#[extendr]
impl ClassMap {
    /// Create a new Class from an R List.
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
    fn set(&mut self, key: String, value: Robj) {
        self.data.borrow_mut().insert(key, value);
    }

    /// Get a value from the Class.
    fn get(&self, key: String) -> Robj {
        self.data
            .borrow()
            .get(&key)
            .unwrap_or(&"ERROR: Key not found".into())
            .clone()
    }

    /// Get the keys of the Class.
    fn keys(&self) -> Vec<String> {
        self.data.borrow().keys().cloned().collect()
    }

    /// Get the values of the Class.
    fn values(&self) -> Vec<Robj> {
        self.data.borrow().values().cloned().collect()
    }

    /// Remove a key-value pair from the Class.
    fn remove(&mut self, key: String) -> Robj {
        self.data
            .borrow_mut()
            .remove(&key)
            .unwrap_or("ERROR: Key not found".into())
    }

    /// Print the Class.
    fn print(&self) {
        println!("{:?}", self.data.borrow());
    }

    /// Retain only the elements specified by the predicate.
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
    /// Clears the map, removing all key-value pairs.
    /// Keeps the allocated memory for reuse.
    fn clear(&mut self) {
        self.data.borrow_mut().clear();
    }

    /// Check for the existence of a key in the Class.
    fn contains_key(&self, key: String) -> bool {
        self.data.borrow().contains_key(&key)
    }

    /// Check for the existence of a key in the Class.
    /// Note: This is an alias for `contains_key`.
    fn has(&self, key: String) -> bool {
        self.data.borrow().contains_key(&key)
    }

    /// Clone the Class.
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
                // let mut _self = List::from_pairs([("map", &definition_map.into())]);
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

        if let Some(post_class) = post.class() {
            let check = post_class.into_iter().any(|c| c == "Class");

            if check {
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

    let mut object = List::from_pairs([("map", definition_map.into())]);
    object.set_class([name, "Class".into()]).unwrap();
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

    // Classy implementation.
    impl Classy;

    // __new_class__ implementation.
    fn __new_class__;
}
