// ============================================================================
// IMPORTS
// ============================================================================

mod types;

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
}

// ============================================================================
// GLOBALS
// ============================================================================

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

// ============================================================================
// STRUCTS
// ============================================================================

#[derive(Debug)]
struct RcRefMap(Rc<RefCell<HashMap<String, Robj>>>);

#[derive(Debug)]
struct RcClassDefinition(Rc<ClassDefinition>);

#[extendr]
#[derive(Debug)]
struct ClassDefinition {
    /// The name of the class.
    name: Strings,

    /// The methods shared by the class instances.
    methods: RcRefMap,
}

#[extendr]
#[derive(Debug)]
struct ClassInstance {
    /// The name of the class.
    name: Strings,

    /// The individual instance data.
    fields: RcRefMap,

    /// Pointer to the class definition this instance belongs to.
    methods: RcClassDefinition,
}

// ============================================================================
// IMPLEMENTATIONS
// ============================================================================

#[extendr]
impl ClassDefinition {
    fn name(&self) -> Strings {
        self.name.clone().into()
    }

    fn new(name: Strings, methods: List) -> Robj {
        let map = list_to_hashmap(methods);

        let class_definition = Rc::new(Self {
            name,
            methods: RcRefMap(Rc::new(RefCell::new(map.into()))),
        });

        ExternalPtr::new(RcClassDefinition(class_definition)).into()
    }

    fn print(&self) {
        println!("ClassDefinition {{");
        println!("    name: {:?}", self.name);
        println!("    methods: {:?}", self.methods.0.borrow());
        println!("}}");
    }

    fn get(&self, key: &str) -> Result<Robj> {
        if let Some(method) = self.methods.0.borrow().get(key) {
            return Ok(method.clone());
        }

        let msg = format!("Method '{}' not found in class '{:?}'", key, self.name);
        Err(Error::Other(msg.into()))
    }
}

fn list_to_hashmap(list: List) -> HashMap<String, Robj> {
    list.into_hashmap()
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect()
}

#[extendr]
impl ClassInstance {
    fn new(name: Strings, fields: List, def: Robj) -> Result<Self> {
        let def_ptr: ExternalPtr<RcClassDefinition> = def.try_into()?;
        let def_rc = RcClassDefinition(Rc::clone(&def_ptr.0));
        // let def_map = def_rc.0.methods.0.borrow();

        let map = list_to_hashmap(fields);

        // for (key, value) in &map {
        //     // Only check validators if method exists
        //     if let Some(validator) = def_map.get(key).and_then(|v| v.as_function()) {
        //         let arg = Pairlist::from_pairs([("", value)]);

        //         if !validator.call(arg)?.as_bool().unwrap_or(false) {
        //             let msg = format!(
        //                 "Invalid type <'{:?}'> passed for field <'{}'>.",
        //                 value.rtype(),
        //                 key
        //             );

        //             return Err(Error::Other(msg.into()));
        //         }
        //     }
        // }

        // // need to explicitly drop the borrow
        // drop(def_map);

        Ok(Self {
            name: name.into(),
            fields: RcRefMap(Rc::new(RefCell::new(map))),
            methods: def_rc,
        })
    }

    fn print(&self) {
        println!("ClassInstance {{");
        println!("    name:   {:?}", self.name);
        println!("    fields: {:?}", self.fields.0.borrow());
        println!("    def:    {:?}", self.methods.0);
        println!("}}");
    }

    fn get(&self, key: &str) -> Result<Robj> {
        // 1. Check instance fields
        if let Some(value) = self.fields.0.borrow().get(key) {
            return Ok(value.clone());
        }

        // 2. Check methods in the class definition
        let def = &self.methods.0;
        if let Some(method) = def.methods.0.borrow().get(key) {
            return Ok(method.clone());
        }

        // 3. If not found
        let msg = format!(
            "Field or method '{}' not found in class instance '{:?}'",
            key, self.name
        );
        Err(Error::Other(msg.into()))
    }

    fn set(&mut self, key: &str, value: Robj) -> Result<Robj> {
        if let Some(value) = self
            .fields
            .0
            .borrow_mut()
            .insert(key.to_string(), value.clone())
        {
            return Ok(value);
        }

        let msg = format!(
            "Unable to set attribute '{:?}' with key '{}' in class '{:?}'",
            value.rtype(),
            key,
            self.name
        );
        Err(Error::Other(msg.into()))
    }
}
