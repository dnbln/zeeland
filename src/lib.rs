pub extern crate serde;

pub use zeeland_derive::zeeland;
pub use async_trait::async_trait;

pub struct ResponseParser<Response, T> {
    _phantom: std::marker::PhantomData<(Response, T)>,
}