// ============================================================================
// IMPORTS
// ============================================================================

mod maps;
use maps::*;

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
static ALLOCATOR: mimalloc::MiMalloc = mimalloc::MiMalloc;

// ============================================================================
// NEW SETUP
// Made a mistake.
// The map defined in define_class
// is the same as the one in initialise_class.
// So if a a second class is created,
// it contains the old map.
//
// Need to:
// 1. Create a map in define_class containing the methods. This contains:
//        - Validators: anything that has class RS_TYPE.
//        - Methods: anything that has ".self" in the formals. We then assign RS_METHOD to these.
//        - Static: anything that has class RS_STATIC.
//        - Class: anything that has class RS_CLASS.
// 2. Create a * new * map in initialise_class containing the attributes, plus the methods.
//        - This must be a new map, not a reference to the old one.
// 3. Send the map back to R with class attributes.
// ============================================================================

#[derive(Debug)]
struct RcClassDefinition(Rc<ClassDefinition>);

#[extendr]
#[derive(Debug)]
struct ClassDefinition {
    /// The name of the class.
    name: &'static str,

    /// The methods shared by the class instances.
    methods: RcRefMap,
}

#[extendr]
#[derive(Debug)]
struct ClassInstance {
    /// The name of the class.
    name: &'static str,

    /// The individual instance data.
    fields: RcRefMap,

    /// Pointer to the class definition this instance belongs to.
    methods: RcClassDefinition,
}

// Allow conversion
impl From<HashMap<String, Robj>> for RobjMap {
    fn from(map: HashMap<String, Robj>) -> Self {
        RobjMap(map)
    }
}

#[extendr]
impl ClassDefinition {
    fn name(&self) -> &'static str {
        self.name
    }

    fn new(name: &'static str, methods: List) -> Robj {
        let map = methods
            .into_hashmap()
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect::<HashMap<_, _>>();

        let methods_map = RcRefMap(Rc::new(RefCell::new(map.into())));
        let class_def = Rc::new(ClassDefinition {
            name,
            methods: methods_map,
        });

        r!(ExternalPtr::new(RcClassDefinition(class_def)))
    }

    fn print(&self) {
        println!("ClassDefinition {{");
        println!("    name: {}", self.name);
        println!("    methods: {:?}", self.methods.0.borrow());
        println!("}}");
    }

    fn get(&self, key: &str) -> Result<Robj> {
        if let Some(method) = self.methods.0.borrow().0.get(key) {
            return Ok(method.clone());
        }
        Err(Error::Other(
            format!("Method '{}' not found in class '{}'", key, self.name).into(),
        ))
    }
}

#[extendr]
impl ClassInstance {
    fn new(name: &'static str, fields: List, def: Robj) -> Result<Self> {
        let def_ptr: ExternalPtr<RcClassDefinition> = def.try_into()?;
        let rc_def = RcClassDefinition(Rc::clone(&def_ptr.0));

        let map = fields
            .into_hashmap()
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect::<HashMap<_, _>>();

        for (key, value) in map.iter() {
            // Allows for composition of classes.
            if value.inherits("ClassInstance") {
                continue;
            }

            let before = rc_def.0.get(key.as_str()).unwrap(); // Use and_then ??

            if let Some(validator) = before.as_function() {
                let arg = Pairlist::from_pairs([("", value.clone())]);

                // let check = match validator.call(arg)?.as_bool() {
                //     Ok(result) => result.as_bool().unwrap_or(false),
                //     Err(_) => {
                //         let msg =
                //             format!("ERROR: Validator function failed for attribute: {}", key);
                //         return Err(Error::Other(msg.into()));
                //     }
                // };

                if validator.call(arg)?.as_bool().unwrap_or(false) {
                    continue;
                }

                let msg = format!(
                    "Invalid type <'{:?}'> passed for field <'{}'>.",
                    value.rtype(),
                    key
                );
                return Err(Error::Other(msg));
            }
        }

        Ok(ClassInstance {
            name,
            fields: RcRefMap(Rc::new(RefCell::new(map.into()))),
            methods: rc_def,
        })
    }

    fn print(&self) {
        println!("ClassInstance {{");
        println!("    name:   {}", self.name);
        println!("    fields: {:?}", self.fields.0.borrow());
        println!("    def:    {:?}", self.methods.0);
        println!("}}");
    }

    fn get(&self, key: &str) -> Result<Robj> {
        // 1. Check instance fields
        if let Some(value) = self.fields.0.borrow().0.get(key) {
            return Ok(value.clone());
        }

        // 2. Check methods in the class definition
        let def = &self.methods.0;
        if let Some(method) = def.methods.0.borrow().0.get(key) {
            return Ok(method.clone());
        }

        // 3. If not found
        let msg = format!(
            "Field or method '{}' not found in class instance '{}'",
            key, self.name
        );
        Err(Error::Other(msg.into()))
    }

    fn set(&mut self, key: &str, value: Robj) -> Result<Robj> {
        if let Some(value) = self
            .fields
            .0
            .borrow_mut()
            .0
            .insert(key.to_string(), value.clone())
        {
            return Ok(value);
        }

        let msg = format!(
            "Unable to set attribute '{:?}' with key '{}' in class '{}'",
            value.rtype(),
            key,
            self.name
        );
        Err(Error::Other(msg.into()))
    }
}
