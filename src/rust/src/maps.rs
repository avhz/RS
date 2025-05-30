use extendr_api::{ExternalPtr, Robj};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub(crate) struct RobjMap1(pub(crate) HashMap<String, Robj>);

#[derive(Debug)]
pub(crate) struct RcRefMap1(pub(crate) Rc<RefCell<RobjMap1>>);

#[derive(Debug)]
pub(crate) struct ExtPtrMap1(pub(crate) ExternalPtr<RcRefMap1>);
