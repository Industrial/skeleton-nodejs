use cfg_if::cfg_if;
use leptos::*;
use thiserror::Error;

#[cfg(feature = "ssr")]
use http::status::StatusCode;
#[cfg(feature = "ssr")]
use leptos_axum::ResponseOptions; // Using http::status::StatusCode to match leptos_axum

#[derive(Clone, Debug, Error)]
pub enum AppError {
  #[error("Not Found")]
  NotFound,
}

impl AppError {
  #[cfg(feature = "ssr")]
  pub fn status_code(&self) -> StatusCode {
    match self {
      AppError::NotFound => StatusCode::NOT_FOUND,
    }
  }
}

#[component]
pub fn ErrorTemplate(
  #[prop(optional)] outside_errors: Option<Errors>,
  #[prop(optional)] errors: Option<RwSignal<Errors>>,
) -> impl IntoView {
  let errors = match outside_errors {
    Some(e) => create_rw_signal(e),
    None => match errors {
      Some(e) => e,
      None => panic!("No Errors found and we expected errors!"),
    },
  };
  let errors = errors.get_untracked();

  let errors: Vec<AppError> = errors
    .into_iter()
    .filter_map(|(_k, v)| v.downcast_ref::<AppError>().cloned())
    .collect();
  println!("Errors: {errors:#?}");

  cfg_if! { if #[cfg(feature="ssr")] {
      let response = use_context::<ResponseOptions>();
      if let Some(response) = response {
          response.set_status(errors[0].status_code());
      }
  }}

  view! {
    <h1>{if errors.len() > 1 { "Errors" } else { "Error" }}</h1>
    <For
      each=move || { errors.clone().into_iter().enumerate() }
      key=|(index, _error)| *index
      children=move |error| {
          let error_string = error.1.to_string();
          #[cfg(feature = "ssr")]
          let error_code = error.1.status_code();
          #[cfg(not(feature = "ssr"))]
          let error_code = "Error";
          view! {
            <h2>{error_code.to_string()}</h2>
            <p>"Error: " {error_string}</p>
          }
      }
    />
  }
}
