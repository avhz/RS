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

    fn class_equality;
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
struct RcRefMap(Rc<RefCell<RobjMap>>);

#[derive(Debug, Clone, PartialEq)]
struct RcClassDefinition(Rc<ClassDefinition>);

#[extendr]
#[derive(Debug, Clone, PartialEq)]
struct ClassDefinition {
    /// The name of the class.
    name: Strings,

    /// The methods shared by the class instances.
    methods: RcRefMap,

    /// Whether to validate the class instance fields.
    validate: bool,
}

#[extendr]
#[derive(Debug, PartialEq)]
struct ClassInstance {
    // /// The name of the class.
    // name: Strings,
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

// TODO: Fix this !
#[extendr]
fn class_equality(class1: ExternalPtr<ClassInstance>, class2: ExternalPtr<ClassInstance>) -> bool {
    class1 == class2
}

impl RcRefMap {
    fn from_hashmap(map: RobjMap) -> Self {
        Self(Rc::new(RefCell::new(map)))
    }

    fn from_list(list: List) -> Self {
        Self::from_hashmap(list_to_robjmap(list))
    }
}

#[extendr]
impl ClassDefinition {
    fn new(name: Strings, methods: List, validate: bool) -> Robj {
        let class_definition = Rc::new(Self {
            name,
            methods: RcRefMap::from_list(methods),
            validate,
        });

        ExternalPtr::new(RcClassDefinition(class_definition)).into()
    }
}

#[extendr]
impl ClassInstance {
    fn new(fields: List, def: Robj) -> Result<Self> {
        let def_ptr: ExternalPtr<RcClassDefinition> = def.try_into()?;
        let def_ref = RcClassDefinition(Rc::clone(&def_ptr.0));
        let def_map = def_ref.0.methods.0.borrow();

        let map = list_to_robjmap(fields);

        if def_ref.0.validate {
            for (key, value) in &map {
                // Support composition of classes
                if value.inherits("ClassInstance") {
                    continue;
                }

                // Only check validators if method exists
                if let Some(validator) = def_map.get(key).and_then(|v| v.as_function()) {
                    let arg = Pairlist::from_pairs([("", value)]);

                    if !validator.call(arg)?.as_bool().unwrap_or(false) {
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

        // need to explicitly drop the borrow
        drop(def_map);

        Ok(Self {
            fields: RcRefMap::from_hashmap(map),
            definition: def_ref,
        })
    }

    fn print(&self) {
        let name = &self.definition.0.name;
        let fields = &self.fields.0.borrow();
        let methods = self.definition.0.methods.0.borrow();

        println!("ClassInstance {:?} @ {:p} {{", name, &self);
        println!("    fields:  {:?}", fields);
        println!("    methods: {:?}", methods.keys());
        println!("}}");
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
        if let Some(method) = (&self.definition.0).methods.0.borrow().get(key) {
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
        let inserted = self
            .fields
            .0
            .borrow_mut()
            .insert(key.to_string(), value.clone());

        if let Some(value) = inserted {
            return Ok(value);
        }

        let msg = format!(
            "Unable to set attribute '{:?}' with key '{}' in class '{:?}'",
            &value, key, self.definition.0.name
        );
        Err(Error::Other(msg.into()))
    }
}
