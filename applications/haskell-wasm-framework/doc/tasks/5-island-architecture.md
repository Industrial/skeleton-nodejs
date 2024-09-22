# Task 5: Implement Island Architecture for Partial Hydration

Description: Introduce support for island architecture, allowing for partial hydration of interactive components on client-side pages. This approach will ensure that only specific parts of the page are hydrated for interactivity, optimizing performance.

Acceptance Criteria:
  - Define a way to annotate interactive components in Haskell that should be hydrated on the client side.
  - Implement client-side logic to find and hydrate these components after the initial HTML is loaded.
  - Ensure only the required JavaScript/WASM for these components is loaded and executed.
  - A demo page with a non-interactive part (e.g., static content) and an interactive island (e.g., a button click counter) is correctly hydrated on the client side.
  - The solution is tested to confirm that only designated components are hydrated and that it improves performance.
