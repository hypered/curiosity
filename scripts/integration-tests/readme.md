# Integration Tests Scripts

This integration test suite is meant to be used to generate an interactive integration test environment and to perform some integration tests on the CI.

You can locally setup an interactive integration env using the `./interactive-integration-env` script. This script build and start a curiosity server and an Nginx reverse proxy.

> ⚠️ Warning
>
> The `.sh` scripts living in this directory are not meant to be executed in isolation. These scripts need to be glued to their dependencies via Nix. They are living outside of `default.nix` for convenience reasons: it improves the editor syntax color highlighting and linting support.
