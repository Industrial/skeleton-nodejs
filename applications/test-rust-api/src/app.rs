use crate::error_template::{AppError, ErrorTemplate};
use leptos::*;
use leptos_meta::*;
use leptos_router::*;

#[component]
pub fn App() -> impl IntoView {
  // Provides context that manages stylesheets, titles, meta tags, etc.
  provide_meta_context();

  view! {
    <Stylesheet id="leptos" href="/pkg/shuttle-leptos.css" />

    // sets the document title
    <Title text="Welcome to Leptos" />

    // content for this welcome page
    <Router fallback=|| {
        let mut outside_errors = Errors::default();
        outside_errors.insert_with_default_key(AppError::NotFound);
        view! { <ErrorTemplate outside_errors /> }.into_view()
    }>
      <main>
        <Routes>
          <Route path="" view=HomePage />
        </Routes>
      </main>
    </Router>
  }
}

/// Renders the home page of your application.
#[component]
fn HomePage() -> impl IntoView {
  // Creates a reactive value to update the button
  let (count, set_count) = create_signal(0);
  let on_click = move |_| set_count.update(|count| *count += 1);

  view! {
    <div>
      <picture>
        <source
          srcset="https://raw.githubusercontent.com/leptos-rs/leptos/main/docs/logos/Leptos_logo_pref_dark_RGB.svg"
          media="(prefers-color-scheme: dark)"
        />
        <img
          src="https://raw.githubusercontent.com/leptos-rs/leptos/main/docs/logos/Leptos_logo_RGB.svg"
          alt="Leptos Logo"
        />
      </picture>
      <h1>"Welcome to Leptos!"</h1>
      <button on:click=on_click>"Click Me: " {count}</button>
    </div>
  }
}
