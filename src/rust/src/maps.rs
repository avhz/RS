// ============================================================================
// IMPORTS
// ============================================================================

use extendr_api::Robj;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

// ============================================================================
// STRUCTS
// ============================================================================

// #[derive(Debug)]
// pub(crate) struct RobjMap(pub(crate) HashMap<String, Robj>);

// #[derive(Debug)]
// pub(crate) struct ExtPtrMap(pub(crate) ExternalPtr<RcRefMap>);

// ============================================================================
// IMPLEMENTATIONS
// ============================================================================

// impl RobjMap {
//     fn new() -> Self {
//         Self(HashMap::new())
//     }
// }

impl RcRefMap {
    fn new() -> Self {
        Self(Rc::new(RefCell::new(HashMap::new())))
    }
}

// impl ExtPtrMap {
//     fn new() -> Self {
//         Self(ExternalPtr::new(RcRefMap::new()))
//     }
// }

// Allow conversion
// impl From<HashMap<String, Robj>> for RobjMap {
//     fn from(map: HashMap<String, Robj>) -> Self {
//         RobjMap(map)
//     }
// }

// impl RcRefMap {
//     /// Create a new empty ClassMap with a capacity.
//     ///
//     /// @export
//     fn with_capacity(capacity: usize) -> Self {
//         Self {
//             data: Rc::new(RefCell::new(HashMap::with_capacity(capacity))),
//         }
//     }

//     /// Create a new ClassMap from a HashMap.
//     ///
//     /// @export
//     fn from_hashmap(map: HashMap<String, Robj>) -> Self {
//         Self {
//             data: Rc::new(RefCell::new(map)),
//         }
//     }

//     /// Create a new Class from an R List.
//     ///
//     /// @export
//     fn from_list(list: List) -> Self {
//         let data = list
//             .into_hashmap()
//             .into_iter()
//             .map(|(k, v)| (k.to_string(), v))
//             .collect();

//         Self {
//             data: Rc::new(RefCell::new(data)),
//         }
//     }

//     /// Set a value in the Class.
//     ///
//     /// @export
//     fn set(&mut self, key: String, value: Robj) {
//         self.data.borrow_mut().insert(key, value);
//     }

//     /// Set multiple values in the Class.
//     ///
//     /// @export
//     fn mset(&mut self, values: List) {
//         for (key, value) in values.into_hashmap() {
//             self.data.borrow_mut().insert(key.to_string(), value);
//         }
//     }

//     /// Get a value from the Class.
//     ///
//     /// @export
//     fn get(&self, key: String) -> Robj {
//         self.data
//             .borrow()
//             .get(&key)
//             .unwrap_or(&"ERROR: Key not found".into())
//             .clone()
//     }

//     /// Get the keys of the Class.
//     ///
//     /// @export
//     fn keys(&self) -> Vec<String> {
//         self.data.borrow().keys().cloned().collect()
//     }

//     /// Get the values of the Class.
//     ///
//     /// @export
//     fn values(&self) -> Vec<Robj> {
//         self.data.borrow().values().cloned().collect()
//     }

//     /// Remove a key-value pair from the Class.
//     ///
//     /// @export
//     fn remove(&mut self, key: String) -> Robj {
//         self.data
//             .borrow_mut()
//             .remove(&key)
//             .unwrap_or("ERROR: Key not found".into())
//     }

//     /// Print the ClassMap.
//     ///
//     /// @export
//     fn print(&self) {
//         println!("{{");
//         for (key, value) in self.data.borrow().iter() {
//             println!("    {}: {:?}", key, value);
//         }
//         println!("}}");
//     }

//     /// Retain only the elements specified by the predicate.
//     ///
//     /// @export
//     fn retain(&mut self, f: Robj) {
//         if !f.is_function() {
//             panic!("ERROR: Expected a function. Got: {:?}", f);
//         }

//         self.data.borrow_mut().retain(|key, value| {
//             f.as_function()
//                 .unwrap()
//                 .call(pairlist!(key.clone(), value.clone()))
//                 .unwrap_or(Robj::from(NA_LOGICAL))
//                 .as_bool()
//                 .unwrap_or(false)
//         });
//     }

//     /// Clear the Class.
//     ///
//     /// Clears the map, removing all key-value pairs.
//     /// Keeps the allocated memory for reuse.
//     ///
//     /// @export
//     fn clear(&mut self) {
//         self.data.borrow_mut().clear();
//     }

//     /// Check for the existence of a key in the Class.
//     ///
//     /// @export
//     fn contains_key(&self, key: String) -> bool {
//         self.data.borrow().contains_key(&key)
//     }

//     /// Check for the existence of a key in the Class.
//     ///
//     /// Note: This is an alias for `contains_key`.
//     ///
//     /// @export
//     fn has(&self, key: String) -> bool {
//         self.data.borrow().contains_key(&key)
//     }

//     /// Clone the Class.
//     ///
//     /// @export
//     fn clone(&self) -> Self {
//         Self {
//             data: self.data.clone(),
//         }
//     }
// }

// impl TryFrom<&Robj> for ClassMap {
//     type Error = Error;

//     fn try_from(value: &Robj) -> Result<Self> {
//         let result: Result<ExternalPtr<Self>> = value.try_into();

//         match result.and_then(|ptr| Ok(ptr.data.clone())) {
//             // Ok(data) => Ok(ClassMap {
//             //     data: Rc::new(RefCell::new(data.borrow().clone())),
//             // }),
//             Ok(data) => Ok(ClassMap { data }),
//             Err(_) => Err(Error::TypeMismatch("Expected ClassMap instance.".into())),
//         }
//     }
// }

// impl From<List> for ClassMap {
//     fn from(list: List) -> Self {
//         let data = list
//             .into_hashmap()
//             .into_iter()
//             .map(|(k, v)| (k.to_string(), v))
//             .collect();

//         Self {
//             data: Rc::new(RefCell::new(data)),
//         }
//     }
// }

// fn _externalptr_to_hashmap(robj: Robj) -> Result<HashMap<String, Robj>> {
//     let ptr: ExternalPtr<HashMap<String, Robj>> = robj
//         .try_into()
//         .map_err(|_| Error::TypeMismatch("Expected ClassMap instance.".into()))?;

//     Ok(ptr.as_ref().clone())
// }
