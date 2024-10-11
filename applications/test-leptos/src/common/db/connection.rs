// EdgeDB connection setup and utilities
use anyhow::Result;
use edgedb_tokio::credentials::Credentials;
use edgedb_tokio::Client;
use std::env;

pub async fn get_client() -> Result<Client> {
  // Load environment variables or specify credentials directly
  let credentials = Credentials::from_env()?;
  let client = Client::connect(credentials).await?;
  Ok(client)
}
