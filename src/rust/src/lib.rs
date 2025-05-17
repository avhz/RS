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

// SPECIAL CLASS NAMES
pub const RS_CLASS: &str = "RS_CLASS";
pub const RS_SELF: &str = "RS_SELF";
// pub const RS_METHOD: &str = "RS_METHOD";
// pub const RS_TYPE: &str = "RS_TYPE";
// pub const RS_STATIC: &str = "RS_STATIC";

// WRAPPER FUNCTIONS TO SEND CONSTS TO R

// #[extendr]
// fn rs_class() -> Rstr {
//     RS_CLASS.into()
// }

// #[extendr]
// fn rs_self() -> Rstr {
//     RS_SELF.into()
// }

// #[extendr]
// fn rs_method() -> Rstr {
//     RS_METHOD.into()
// }

// #[extendr]
// fn rs_type() -> Rstr {
//     RS_TYPE.into()
// }

// #[extendr]
// fn rs_static() -> Rstr {
//     RS_STATIC.into()
// }

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

    // /// Get multiple values from the Class.
    // ///
    // /// @export
    // fn mget(&self, keys: Strings) -> Result<List> {
    //     let mut result = HashMap::with_capacity(keys.len());

    //     for key in keys.iter() {
    //         if let Some(value) = self.data.borrow().get(key.to_string()) {
    //             result.insert(key.to_string(), value.clone());
    //         }
    //     }
    //     List::from_hashmap(result)
    // }

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

fn externalptr_to_hashmap(robj: Robj) -> Result<HashMap<String, Robj>> {
    let ptr: ExternalPtr<HashMap<String, Robj>> = robj
        .try_into()
        .map_err(|_| Error::TypeMismatch("Expected ClassMap instance.".into()))?;

    Ok(ptr.as_ref().clone())
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
#[derive(Debug)]
struct Class {
    /// Name of the class.
    name: String,

    /// Map containing the methods.
    methods: HashMap<String, Robj>,

    /// Map containing the static methods.
    statics: HashMap<String, Robj>,

    /// Map containing the validators (aka types).
    validators: HashMap<String, Robj>,

    /// Map containing the class instance.
    instance: Rc<RefCell<HashMap<String, Robj>>>,
}

// type RobjMap = ExternalPtr<HashMap<String, Robj>>;

// fn create_object_2(
//     map: HashMap<String, Robj>,
//     class: [&str; 2],
// ) -> Result<ExternalPtr<HashMap<String, Robj>>> {
//     let mut object = ExternalPtr::new(map);
//     object.set_class(class)?;
//     Ok(object)
// }

// fn create_method_2(map: &mut HashMap<String, Robj>, self_: &RobjMap, key: &str) -> Result<()> {
//     if let Some(method_fn) = map.get(key).unwrap().as_function() {
//         if let (Some(body), Some(formals), Some(environment)) = (
//             method_fn.body().and_then(|b| b.as_language()),
//             method_fn.formals(),
//             method_fn.environment(),
//         ) {
//             let new_formals = Pairlist::from_pairs(
//                 &formals
//                     .iter()
//                     .filter(|(k, _)| *k != ".self") // Exclude .self from formals
//                     .collect::<Vec<_>>(),
//             );

//             environment.set_local(Symbol::from_string(".self"), self_.clone());

//             match Function::from_parts(new_formals, body, environment) {
//                 Ok(new_method) => {
//                     map.insert(key.into(), new_method.into());
//                 }
//                 Err(err) => {
//                     let msg = format!("ERROR: Failed to create method: {}", err);
//                     return Err(Error::Other(msg.into()));
//                 }
//             }
//         }
//     }
//     Ok(())
// }

// // Function to assign correct class to an Robj
// fn assign_class(robj: &mut Robj, class: &str) -> Result<()> {
//     robj.set_class([class])?;
//     Ok(())
// }

// // .self <- .Call(
// //     "wrap__define_class",
// //     name = .classname,
// //     definition = definition,
// //     PACKAGE = "RS"
// // )
// #[extendr]
// fn define_class_2(classname: &str, definition: List) -> Result<RobjMap> {
//     // Create a new basic hashmap from the definition arguments
//     // This contains the validators, methods, and static methods.
//     let mut definition: HashMap<String, Robj> = definition
//         .into_hashmap()
//         .into_iter()
//         .map(|(k, v)| (k.to_string(), v))
//         .collect();

//     let self_ = create_object_2(definition.clone(), [RS_SELF, classname])?;
//     methods.iter().for_each(|key| {
//         let _ = create_method_2(&mut definition, &self_, &key);
//     });
//     Ok(self_)
// }

// fn validate_attribute_2(data: &mut HashMap<String, Robj>, key: String, after: Robj) -> Result<()> {
//     // Fast path for Class composition
//     if after.inherits(RS_CLASS) {
//         data.insert(key, after);
//         return Ok(());
//     }

//     if let Some(validator) = data.get(&key).and_then(|before| before.as_function()) {
//         let arg = Pairlist::from_pairs([("", after.clone())]);

//         let check = match validator.call(arg) {
//             Ok(result) => result.as_bool().unwrap_or(false),
//             Err(_) => {
//                 let msg = format!("ERROR: Validator function failed for attribute: {}", key);
//                 return Err(Error::Other(msg.into()));
//             }
//         };

//         if check {
//             data.insert(key, after);
//             return Ok(());
//         }

//         let msg = format!(
//             "Invalid type <'{:?}'> passed for field <'{}'>.",
//             after.rtype(),
//             key
//         );
//         return Err(Error::TypeMismatch(msg.into()));
//     }

//     Ok(())
// }

// #[extendr]
// fn initialise_class_2(name: &str, self_: Robj, instance_args: List) -> Result<List> {
//     let classmap = match externalptr_to_hashmap(self_) {
//         Ok(robj) => ClassMap::from_hashmap(robj),
//         Err(_) => {
//             let msg = "ERROR: Could not convert self_ to ClassMap.";
//             return Err(Error::Other(msg.into()));
//         }
//     };

//     let data_ref = Rc::clone(&classmap.data);
//     let mut data = data_ref.borrow_mut();

//     for (key, after) in instance_args.into_hashmap() {
//         validate_attribute_2(&mut data, key.to_string(), after)?;
//     }

//     let mut object = List::from_pairs([("map", ClassMap::from_hashmap(data.clone()).into())]);
//     object.set_class([RS_CLASS, name])?;
//     Ok(object)
// }

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

    fn chatgpt;

    // fn define_class_3;
    // fn initialise_class_3;
    // fn rs_class;
    // fn rs_self;
    // fn rs_method;
    // fn rs_type;
    // fn rs_static;
}

// ============================================================================
// SETUP 3
// ============================================================================

// type RcRefMap = Rc<RefCell<HashMap<String, Robj>>>;
// type ExtPtrMap = ExternalPtr<RcRefMap>;

// fn create_object_3(map: ClassMap, class: [&str; 2]) -> Result<List> {
//     let mut object = List::from_pairs([("map", map.into())]);
//     object.set_class(class)?;
//     Ok(object)
// }

// #[extendr]
// fn define_class_3(name: &str, definition: List) -> Result<()> {
//     // Create new map for the class
//     let mut map: RcRefMap = Rc::new(RefCell::new(HashMap::with_capacity(definition.len())));

//     // Get the definition map from the List
//     let mut definition = definition
//         .into_hashmap()
//         .into_iter()
//         .map(|(k, v)| (k.to_string(), v))
//         .collect::<HashMap<String, Robj>>();

//     // Get a mutable reference to the map
//     let data_ref = Rc::clone(&map);
//     let mut data = data_ref.borrow_mut();

//     for (key, value) in definition.iter() {
//         let item = data
//             .get(key)
//             .unwrap_or(&"ERROR: Key not found".into())
//             .clone();
//         // If METHOD
//         if value.inherits("RS_METHOD") {
//             if let Some(method_fn) = definition.get(key).as_function() {
//                 if let (Some(body), Some(formals), Some(environment)) = (
//                     method_fn.body().and_then(|b| b.as_language()),
//                     method_fn.formals(),
//                     method_fn.environment(),
//                 ) {
//                     let new_formals = Pairlist::from_pairs(
//                         &formals
//                             .iter()
//                             .filter(|(k, _)| *k != ".self") // Exclude .self from formals
//                             .collect::<Vec<_>>(),
//                     );

//                     environment.set_local(Symbol::from_string(".self"), self_);

//                     match Function::from_parts(new_formals, body, environment) {
//                         Ok(new_method) => map.set(key.into(), new_method.into()),
//                         Err(err) => {
//                             let msg = format!("ERROR: Failed to create method: {}", err);
//                             return Err(Error::Other(msg.into()));
//                         }
//                     }
//                 }
//             }
//         }
//         // If STATIC
//         else if value.inherits("RS_STATIC") {
//         }
//         // If TYPE
//         else if value.inherits("RS_TYPE") {
//         }
//         // If CLASS
//         else if value.inherits("RS_CLASS") {
//             // Create a new ClassMap for the class
//             let classmap = ClassMap::from_list(value.as_list().unwrap());
//             map.borrow_mut().insert(key.to_string(), classmap.into());
//         }
//     }

//     Ok(())

//     // let mut definition_map: ClassMap = definition.into();
//     // let self_ = create_object_3(definition_map.clone(), [RS_SELF, name])?;

//     // methods.iter().for_each(|key| {
//     //     if let Some(method_fn) = definition_map.get(key.to_string()).as_function() {
//     //         if let (Some(body), Some(formals), Some(environment)) = (
//     //             method_fn.body().and_then(|b| b.as_language()),
//     //             method_fn.formals(),
//     //             method_fn.environment(),
//     //         ) {
//     //             let new_formals = Pairlist::from_pairs(
//     //                 &formals
//     //                     .iter()
//     //                     .filter(|(k, _)| *k != ".self") // Exclude .self from formals
//     //                     .collect::<Vec<_>>(),
//     //             );

//     //             environment.set_local(Symbol::from_string(".self"), &self_);

//     //             if let Ok(method) = Function::from_parts(new_formals, body, environment) {
//     //                 definition_map.set(key.to_string(), method.into());
//     //             }
//     //         }
//     //     }
//     // });

//     // Ok(self_)
// }

// // ============================================================================
// // INITIALISE CLASS FUNCTION
// // ============================================================================

// #[extendr]
// fn initialise_class_3(name: &str, self_: List, instance_args: List) -> Result<List> {
//     let classmap = match self_.into_hashmap().get("map") {
//         Some(robj) => ClassMap::try_from(robj)?,
//         None => {
//             let msg = "ERROR: Missing 'map' in self_ argument to initialise_class.";
//             return Err(Error::Other(msg.into()));
//         }
//     };

//     let data_ref = Rc::clone(&classmap.data);
//     let mut data = data_ref.borrow_mut();

//     for (key, after) in instance_args.into_hashmap() {
//         let key = key.to_string();

//         // Fast path for Class composition
//         if after.inherits(RS_CLASS) {
//             data.insert(key, after);
//             continue;
//         }

//         if let Some(validator) = data.get(&key).and_then(|before| before.as_function()) {
//             let arg = Pairlist::from_pairs([("", after.clone())]);

//             let check = match validator.call(arg) {
//                 Ok(result) => result.as_bool().unwrap_or(false),
//                 Err(_) => {
//                     let msg = format!("ERROR: Validator function failed for attribute: {}", key);
//                     return Err(Error::Other(msg.into()));
//                 }
//             };

//             if check {
//                 data.insert(key, after);
//                 continue;
//             }

//             let msg = format!(
//                 "Invalid type <'{:?}'> passed for field <'{}'>.",
//                 after.rtype(),
//                 key
//             );
//             return Err(Error::TypeMismatch(msg.into()));
//         }
//     }

//     Ok(create_object_3(classmap, [RS_CLASS, name])?)
// }

/// Define a new class by injecting it into the global R environment.
#[extendr]
fn chatgpt(name: &str, members: List) -> Robj {
    // This is your "class dictionary" (like Foo.__dict__)
    let class_env = Environment::new_with_parent(global_env());

    for (k, v) in members.iter() {
        class_env.set_local(k, v);
    }

    // R code to create a constructor function that:
    // - creates a new env with parent = class_env
    // - sets class to "PyObject"
    // - calls `__init__` if defined
    let constructor_code = r#"
        function(...) {
            inst <- new.env(parent = class_env)
            class(inst) <- "PyObject"
            if (!is.null(inst$`__init__`)) {
              inst$`__init__`(inst, ...)
            }
            inst
        }
    "#;

    // Evaluate the R constructor code and inject class_env into its closure
    let constructor = R!(constructor_code).unwrap();

    let constructor_env = constructor.as_function().unwrap().environment().unwrap();

    constructor_env.set_local("class_env", class_env.clone());

    global_env().set_local(name, constructor);

    Robj::from(class_env)
}
