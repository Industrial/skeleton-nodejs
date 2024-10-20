pub mod array;
pub mod command;
pub mod env_var;
pub mod file;
pub mod hash_map;
pub mod hash_set;
pub mod interval;
pub mod iterator;
pub mod json_string;
pub mod mpsc_receiver;
pub mod random_number;
pub mod string;
pub mod tcp_stream;
pub mod timeout;
pub mod vec;

use futures::Stream;
use std::pin::Pin;

pub trait Source<I> {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = I> + Send>>;
}
