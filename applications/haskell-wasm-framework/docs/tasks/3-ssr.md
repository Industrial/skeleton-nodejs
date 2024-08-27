#Task 3: Develop Server-Side Rendering (SSR) Capability

Description: Add support for server-side rendering of Haskell-based views. The server should be able to pre-render pages before sending them to the client, ensuring faster load times and better SEO.

Acceptance Criteria:
  - The server can render a Haskell view to HTML on the server-side using BunJS.
  - Implement a function that converts Haskell data types into HTML strings.
  - Ensure that the rendered HTML can be served directly as a response to HTTP requests.
  - A sample page (e.g., Home page) is correctly rendered on the server and sent to the client.
  - The SSR functionality is tested to verify that the HTML output is as expected.
