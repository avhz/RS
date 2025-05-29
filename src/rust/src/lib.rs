// ============================================================================
// IMPORTS
// ============================================================================

use extendr_api::{pairlist, prelude::*};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// ============================================================================
// GLOBALS
// ============================================================================

#[global_allocator]
static ALLOCATOR: mimalloc::MiMalloc = mimalloc::MiMalloc;

type RobjMap = HashMap<String, Robj>;
type RcRefMap = Rc<RefCell<RobjMap>>;
type ExtPtrMap = ExternalPtr<RcRefMap>;

// SPECIAL CLASS NAMES
// WRAPPER FUNCTIONS TO SEND CONSTS TO R

#[extendr]
fn rs_class() -> Strings {
    "RS_CLASS".into()
}

#[extendr]
fn rs_self() -> Strings {
    "RS_SELF".into()
}

#[extendr]
fn rs_method() -> Strings {
    "RS_METHOD".into()
}

#[extendr]
fn rs_type() -> Strings {
    "RS_TYPE".into()
}

#[extendr]
fn rs_static() -> Strings {
    "RS_STATIC".into()
}

// ============================================================================
// CLASSMAP
// ============================================================================

#[extendr]
#[derive(Debug)]
struct ClassMap {
    data: RcRefMap,
}

#[extendr]
impl ClassMap {
    /// Create a new empty ClassMap with a capacity.
    ///
    /// @export
    fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Rc::new(RefCell::new(HashMap::with_capacity(capacity))),
        }
    }

    /// Create a new ClassMap (empty).
    ///
    /// @export
    fn new() -> Self {
        Self {
            data: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    /// Create a new ClassMap from a HashMap.
    ///     
    /// @export
    fn from_hashmap(map: HashMap<String, Robj>) -> Self {
        Self {
            data: Rc::new(RefCell::new(map)),
        }
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
    fn mset(&mut self, values: List) {
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
            // Ok(data) => Ok(ClassMap {
            //     data: Rc::new(RefCell::new(data.borrow().clone())),
            // }),
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

fn _externalptr_to_hashmap(robj: Robj) -> Result<HashMap<String, Robj>> {
    let ptr: ExternalPtr<HashMap<String, Robj>> = robj
        .try_into()
        .map_err(|_| Error::TypeMismatch("Expected ClassMap instance.".into()))?;

    Ok(ptr.as_ref().clone())
}

// ============================================================================
// DEFINE CLASS FUNCTION
// ============================================================================

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

fn create_object(map: ClassMap, class: [&str; 2]) -> Result<List> {
    let mut object = List::from_pairs([("map", map.into())]);
    object.set_class(class)?;
    Ok(object)
}

#[extendr]
fn define_class(name: &str, definition_args: List, methods: Strings) -> Result<List> {
    let mut definition_map: ClassMap = definition_args.into();

    let self_ = create_object(definition_map.clone(), ["RS_SELF", name])?;

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
    if after.inherits("RS_CLASS") {
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

    Ok(create_object(classmap, ["RS_CLASS", name])?)
}

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

#[extendr]
fn __new_class__(
    name: &str,
    definition_args: List,
    instance_args: List,
    methods: Strings,
) -> Result<List> {
    let mut definition_map = ClassMap::from_list(definition_args);

    let mut self_ = List::from_pairs([("map", definition_map.clone().into())]);
    self_.set_class([name, rs_self().first().unwrap()]).unwrap();

    for key in methods.iter() {
        let key = key.to_string();

        let method = definition_map.get(key.clone());

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

                environment.set_local(Symbol::from_string(".self"), &self_);

                if let Ok(new_method) = Function::from_parts(new_formals, body, environment) {
                    definition_map.set(key, new_method.into());
                }
            }
        }
    }

    for (key, value) in instance_args.into_hashmap() {
        let after = value;

        // Allows for composition of classes.
        if after.inherits(rs_class().first().unwrap()) {
            definition_map.set(key.into(), after.into());
            continue;
        }

        let before = definition_map.get(key.into());

        if let Some(validator_fn) = before.as_function() {
            let arg = Pairlist::from_pairs([("", after.clone())]);

            let check = validator_fn
                .call(arg)
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
            return Err(Error::Other(msg));
        }
    }

    self_.set_class([name, rs_class().first().unwrap()]);
    Ok(self_)
}

// ============================================================================
// EXTENDR MODULE
// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
// ============================================================================

extendr_module! {
    mod RS;

    impl ClassMap;
    impl RSClass;

    fn __new_class__;
    fn define_class;
    fn initialise_class;

    fn rs_class;
    fn rs_self;
    fn rs_method;
    fn rs_type;
    fn rs_static;
}

// ============================================================================
// E.O.F.
// ============================================================================

#[extendr]
#[derive(Debug)]
struct RSClass {
    /// Name of the class.
    name: &'static str,

    /// Fields of the class.
    fields: RobjMap,

    /// Map containing the methods (".self" in formals).
    methods: RobjMap,

    /// Map containing the static methods (regular functions).
    statics: RobjMap,

    /// Map containing the validators (aka "RS_TYPE"s).
    validators: RobjMap,
}

impl RSClass {
    /// Create a new empty RSClass with a capacity and a name.
    fn with_capacity(name: &'static str, capacity: usize) -> Self {
        RSClass {
            name: name,
            fields: HashMap::with_capacity(capacity),
            methods: HashMap::with_capacity(capacity),
            statics: HashMap::with_capacity(capacity),
            validators: HashMap::with_capacity(capacity),
        }
    }
}

#[extendr]
impl RSClass {
    /// Define a new Class object.
    fn define(name: &'static str, arguments: List) -> Result<Self> {
        let mut object = RSClass::with_capacity(name, arguments.len());

        Ok(object)
    }

    /// Initialise a Class object.
    fn initialise(&self, arguments: List) -> Result<()> {
        let mut data = self.fields.clone();

        for (key, value) in arguments.into_hashmap() {
            data.insert(key.to_string(), value);
        }

        Ok(())
    }
}

// #[extendr]
// fn rs_class_define(name: &str, definition: List) -> Result<RSClass> {
//     let mut methods = HashMap::new();
//     let mut statics = HashMap::new();
//     let mut validators = HashMap::new();

//     for (key, value) in definition.into_hashmap() {
//         if value.inherits(rs_method().first().unwrap()) {
//             methods.insert(key.to_string(), value);
//         } else if value.inherits(rs_static().first().unwrap()) {
//             statics.insert(key.to_string(), value);
//         } else if value.inherits(rs_type().first().unwrap()) {
//             validators.insert(key.to_string(), value);
//         }
//     }

//     let instance = Rc::new(RefCell::new(HashMap::new()));

//     Ok(RSClass {
//         name,
//         methods,
//         statics,
//         validators,
//         instance,
//     })
// }

// ============================================================================
// DEFINE CLASS FUNCTION
// ============================================================================

fn create_method_1(map: &mut ClassMap, self_: &List, key: &str) -> Result<()> {
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

fn create_object_1(map: ClassMap, class: [&str; 2]) -> Result<List> {
    let mut object = List::from_pairs([("map", map.into())]);
    object.set_class(class)?;
    Ok(object)
}

#[extendr]
fn define_class_1(name: &str, definition_args: List, methods: Strings) -> Result<List> {
    let mut definition_map: ClassMap = definition_args.into();

    let self_ = create_object_1(definition_map.clone(), ["RS_SELF", name])?;

    methods.iter().for_each(|key| {
        let _ = create_method_1(&mut definition_map, &self_, &key);
    });

    Ok(self_)
}

// ============================================================================
// INITIALISE CLASS FUNCTION
// ============================================================================

fn validate_attribute_1(data: &mut HashMap<String, Robj>, key: String, after: Robj) -> Result<()> {
    // Fast path for Class composition
    if after.inherits("RS_CLASS") {
        data.insert(key, after);
        return Ok(());
    }

    if let Some(validator) = data.get(&key).and_then(|before| before.as_function()) {
        let arg = pairlist!(after.clone());

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

fn deep_copy_self(name: &str, self_: &List) -> Result<List> {
    let original_classmap: ClassMap = self_
        .clone()
        .into_hashmap()
        .get("map")
        .unwrap()
        .try_into()?;
    let cloned_data = Rc::new(RefCell::new(original_classmap.data.borrow().clone()));
    let new_classmap = ClassMap { data: cloned_data };
    create_object_1(new_classmap, ["RS_SELF", name])
}

#[extendr]
fn initialise_class_1(name: &str, self_: List, instance_args: List) -> Result<List> {
    // Get a deep copy of the map 'self_' argument.
    // This is a deep copy of the map, so we can modify it without affecting the original.
    let self_ = deep_copy_self(name, &self_)?;

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
        // println!("key: {:?}", &key);
        // println!("after: {:?}", &after);

        validate_attribute_1(&mut data, key.to_string(), after)?;
    }

    Ok(create_object_1(classmap, ["RS_CLASS", name])?)
}
