use crate::streamweave::source::Source;
use futures::{Stream, stream};
use std::pin::Pin;
use std::string::String;

// Streams characters or lines from a String.
pub struct StringSource {
  data: String,
  by_line: bool,
}

impl StringSource {
  pub fn new(
    data: String,
    by_line: bool,
  ) -> Self {
    StringSource { data, by_line }
  }

  pub fn from_chars(data: String) -> Self {
    StringSource {
      data,
      by_line: false,
    }
  }

  pub fn from_lines(data: String) -> Self {
    StringSource {
      data,
      by_line: true,
    }
  }
}

impl Source<String> for StringSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>> {
    if self.by_line {
      let lines = self.data.lines().map(String::from).collect::<Vec<_>>();
      Box::pin(stream::iter(lines))
    } else {
      let chars = self.data.chars().map(|c| c.to_string()).collect::<Vec<_>>();
      Box::pin(stream::iter(chars))
    }
  }
}
