pub mod environment_variable;
pub mod global_variable;

pub use global_variable::GlobalVariable;

pub trait Source {
  fn create_stream(&self) -> wasm_streams::ReadableStream;
}
