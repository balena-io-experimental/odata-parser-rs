use std::cell::Cell;
use std::fmt::Debug;
use std::ops::Deref;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Identity;

impl<'a, T> Resolve<'a, T> for Identity {
    type Root = T;
    fn resolve(&self, root: &'a T) -> &'a T {
        root
    }
}

pub trait Resolve<'a, T> {
    type Root;
    fn resolve(&self, root: &'a Self::Root) -> &'a T;
}

#[derive(Clone, Eq, PartialEq)]
pub struct DerefCell<'a, T, S: Resolve<'a, T> = Identity> {
    value: Cell<Option<&'a T>>,
    state: S,
}

impl<'a, T> DerefCell<'a, T, Identity> {
    pub fn new() -> Self {
        DerefCell {
            value: Cell::new(None),
            state: Identity,
        }
    }
}

impl<'a, T, S: Resolve<'a, T>> DerefCell<'a, T, S> {
    pub fn with_state(state: S) -> Self {
        DerefCell {
            value: Cell::new(None),
            state,
        }
    }

    pub fn link(&self, root: &'a <S as Resolve<'a, T>>::Root) {
        match self.value.get() {
            Some(_) => panic!(),
            None => self.value.set(Some(self.state.resolve(root))),
        }
    }
}

impl<'a, T, S: Resolve<'a, T>> Deref for DerefCell<'a, T, S> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.value.get().unwrap()
    }
}

impl<'a, T, S: Resolve<'a, T> + Debug> Debug for DerefCell<'a, T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        Debug::fmt(&self.state, f)
    }
}
