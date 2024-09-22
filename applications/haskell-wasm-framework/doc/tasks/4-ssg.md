# Task 4: Enable Static Site Generation (SSG)

Description: Implement static site generation to pre-render pages at build time. This allows for generating static HTML files that can be served without the need for server processing.

Acceptance Criteria:
  - A build script or command is implemented to pre-render Haskell views into static HTML files.
  - The system should generate static files for all static routes defined in the routing system.
  - Generated static files are correctly stored in a designated output directory (e.g., dist/).
  - The static files can be served directly by a web server without requiring server-side processing.
  - Basic static site generation is tested with sample routes and outputs expected HTML files.
