# Task 1: Setup Project Structure and Initial Tooling

Description: Establish the foundational project structure and configure the necessary tooling to support Haskell development targeting WASM and execution in BunJS. This includes setting up directories, basic configuration files, and ensuring compatibility between Haskell, WASM, and BunJS.

Acceptance Criteria:
  - A directory structure for the framework is created, including separate directories for client-side and server-side code.
  - The Haskell compiler (e.g., GHC with the WASM backend or Asterius) is configured to compile Haskell to WASM.
  - BunJS is integrated as the runtime environment for executing both server and client code.
  - Basic build scripts (using make, stack, or cabal) are set up to compile Haskell code to WASM and to run it with BunJS.
  - Basic "Hello, World!" Haskell application runs successfully on both the client (WASM) and server (BunJS).
