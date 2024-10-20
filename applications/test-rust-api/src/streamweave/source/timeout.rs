use crate::streamweave::source::Source;
use futures::Stream;
use std::pin::Pin;
use std::time::Duration;

// Waits for a specified duration before emitting a single event.
pub struct TimeoutSource {
  delay_duration: Duration,
  message: String,
}

impl TimeoutSource {
  pub fn new(
    delay_duration: Duration,
    message: impl Into<String>,
  ) -> Self {
    TimeoutSource {
      delay_duration,
      message: message.into(),
    }
  }
}

impl Source<String> for TimeoutSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>> {
    let delay_duration = self.delay_duration;
    let message = self.message.clone();

    Box::pin(async_stream::stream! {
        tokio::time::sleep(delay_duration).await;
        yield message;
    })
  }
}
