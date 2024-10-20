use crate::streamweave::source::Source;
use futures::Stream;
use std::pin::Pin;
use std::time::Duration;

// Emits time-based events using tokio::time::interval.
pub struct IntervalSource {
  interval_duration: Duration,
}

impl IntervalSource {
  pub fn new(interval_duration: Duration) -> Self {
    IntervalSource { interval_duration }
  }
}

impl Source<String> for IntervalSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = String> + Send>> {
    let mut interval = tokio::time::interval(self.interval_duration);

    Box::pin(async_stream::stream! {
        loop {
            interval.tick().await;  // Wait for the next interval
            yield "Interval tick".to_string();  // Emit an event/message
        }
    })
  }
}
