#!/usr/bin/env bash
# set -euo pipefail
# exec > >(tee -i "log/entrypoint.log") 2>&1

# echo "> entrypoint"

# echo $(which bun)

# echo $(bun --help)

# bun nx --help

bun nx run @code9/test-solid-start-bun-effect:nx-start

# echo "> entrypoint > finished"
