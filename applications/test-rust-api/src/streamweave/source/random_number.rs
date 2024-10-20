use crate::streamweave::source::Source;
use futures::Stream;
use rand::Rng;
use std::pin::Pin;
use std::time::Duration;
use tokio::time;

// Continuously emits random numbers at a specified interval.
pub struct RandomNumberSource {
  interval_duration: Duration,
}

impl RandomNumberSource {
  pub fn new(interval_duration: Duration) -> Self {
    RandomNumberSource { interval_duration }
  }
}

impl Source<u64> for RandomNumberSource {
  fn stream(&self) -> Pin<Box<dyn Stream<Item = u64> + Send>> {
    let interval_duration = self.interval_duration;

    Box::pin(async_stream::stream! {
        let mut interval = time::interval(interval_duration);
        loop {
            interval.tick().await;  // Wait for the next interval
            let random_number = rand::thread_rng().gen::<u64>();  // Generate a random number
            yield random_number;  // Emit the random number
        }
    })
  }
}
