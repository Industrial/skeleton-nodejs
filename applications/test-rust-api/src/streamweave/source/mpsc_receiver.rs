use crate::streamweave::source::Source;
use futures::Stream;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

// Streams messages received from a tokio::sync::mpsc::Receiver.
pub struct MpscReceiverSource<T> {
  receiver: Arc<Mutex<mpsc::Receiver<T>>>,
}

impl<T> MpscReceiverSource<T> {
  pub fn new(receiver: mpsc::Receiver<T>) -> Self {
    MpscReceiverSource {
      receiver: Arc::new(Mutex::new(receiver)),
    }
  }
}

impl<T> Source<T> for MpscReceiverSource<T>
where
  T: Send + 'static,
{
  fn stream(&self) -> Pin<Box<dyn Stream<Item = T> + Send>> {
    let receiver = Arc::clone(&self.receiver);

    Box::pin(async_stream::stream! {
        let mut recv = receiver.lock().await;
        while let Some(msg) = recv.recv().await {
            yield msg;
        }
    })
  }
}
