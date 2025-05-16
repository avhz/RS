// ============================================================================
// IMPORTS
// ============================================================================

use extendr_api::{pairlist, prelude::*};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// ============================================================================
// EXTENDR MODULE
// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
// ============================================================================

extendr_module! {
    mod RS;

    impl ClassMap;

    fn define_class;
    fn initialise_class;
}

// ============================================================================
// GLOBALS
// ============================================================================

#[global_allocator]
static ALLOCATOR: mimalloc::MiMalloc = mimalloc::MiMalloc;

const RS_CLASS: &str = "RS_CLASS";
const RS_SELF: &str = "RS_SELF";

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

    /// Print the ClassMap.
    ///
    /// @export
    fn print(&self) {
        println!("{{");
        for (key, value) in self.data.borrow().iter() {
            println!("    {}: {:?}", key, value);
        }
        println!("}}");
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

impl TryFrom<&Robj> for ClassMap {
    type Error = Error;

    fn try_from(value: &Robj) -> Result<Self> {
        let result: Result<ExternalPtr<Self>> = value.try_into();

        match result.and_then(|ptr| Ok(ptr.data.clone())) {
            Ok(data) => Ok(ClassMap { data }),
            Err(_) => Err(Error::TypeMismatch("Expected ClassMap instance.".into())),
        }
    }
}

impl From<List> for ClassMap {
    fn from(list: List) -> Self {
        let data = list
            .into_hashmap()
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect();

        Self {
            data: Rc::new(RefCell::new(data)),
        }
    }
}

// ============================================================================
// DEFINE CLASS FUNCTION
// ============================================================================

fn create_object(map: ClassMap, class: [&str; 2]) -> Result<List> {
    let mut object = List::from_pairs([("map", map.into())]);
    object.set_class(class)?;

    Ok(object)
}

fn create_method(map: &mut ClassMap, self_: &List, key: &str) -> Result<()> {
    if let Some(method_fn) = map.get(key.to_string()).as_function() {
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

            environment.set_local(Symbol::from_string(".self"), self_);

            match Function::from_parts(new_formals, body, environment) {
                Ok(new_method) => map.set(key.into(), new_method.into()),
                Err(err) => {
                    let msg = format!("ERROR: Failed to create method: {}", err);
                    return Err(Error::Other(msg.into()));
                }
            }
        }
    }
    Ok(())
}

#[extendr]
fn define_class(name: &str, definition_args: List, methods: Strings) -> Result<List> {
    let mut definition_map: ClassMap = definition_args.into();

    let self_ = create_object(definition_map.clone(), [RS_SELF, name])?;

    methods.iter().for_each(|key| {
        let _ = create_method(&mut definition_map, &self_, &key);
    });

    Ok(self_)
}

// ============================================================================
// INITIALISE CLASS FUNCTION
// ============================================================================

fn validate_attribute(data: &mut HashMap<String, Robj>, key: String, after: Robj) -> Result<()> {
    // Fast path for Class composition
    if after.inherits(RS_CLASS) {
        data.insert(key, after);
        return Ok(());
    }

    if let Some(validator) = data.get(&key).and_then(|before| before.as_function()) {
        let arg = Pairlist::from_pairs([("", after.clone())]);

        let check = match validator.call(arg) {
            Ok(result) => result.as_bool().unwrap_or(false),
            Err(_) => {
                let msg = format!("ERROR: Validator function failed for attribute: {}", key);
                return Err(Error::Other(msg.into()));
            }
        };

        if check {
            data.insert(key, after);
            return Ok(());
        }

        let msg = format!(
            "Invalid type <'{:?}'> passed for field <'{}'>.",
            after.rtype(),
            key
        );
        return Err(Error::TypeMismatch(msg.into()));
    }

    Ok(())
}

#[extendr]
fn initialise_class(name: &str, self_: List, instance_args: List) -> Result<List> {
    let classmap = match self_.into_hashmap().get("map") {
        Some(robj) => ClassMap::try_from(robj)?,
        None => {
            let msg = "ERROR: Missing 'map' in self_ argument to initialise_class.";
            return Err(Error::Other(msg.into()));
        }
    };

    let data_ref = Rc::clone(&classmap.data);
    let mut data = data_ref.borrow_mut();

    for (key, after) in instance_args.into_hashmap() {
        validate_attribute(&mut data, key.to_string(), after)?;
    }

    Ok(create_object(classmap, [RS_CLASS, name])?)
}
