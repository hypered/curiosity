#! /usr/bin/env bash

# This script builds the new system, uploads it and activates it on the target
# system. This is similar to the following blog post:
# https://vaibhavsagar.com/blog/2019/08/22/industrial-strength-deployments/.

# The NIX_SSHOPTS env variable is used by the CD system to inject some
# ssh CLI flags. The PROFILE_PATH can also be set by the CD to prevent
# some rebuild time. See the .github/workflogs/ci.yml file and man
# nix-copy-cloruse for more informations.

set -euo pipefail

nix_args=(
)

# This is the curiosity-1 host at Digital Ocean.
TARGET="root@smartcoop.sh"

PROFILE_PATH="${PROFILE_PATH:-}"
PROFILE_PATH="${NIX_SSHOPTS:-}"
# The profile path could be injected by the CD.
if [ -z "${PROFILE_PATH}" ]; then
    echo Building toplevel...
    PROFILE_PATH="$(nix-build -A toplevel --no-out-link)"
    echo "${PROFILE_PATH}"
fi

echo Copying toplevel closure to target system...
nix-copy-closure --to --use-substitutes $TARGET "${PROFILE_PATH}"

echo Activating copied toplevel...
ssh ${NIX_SSHOPTS} $TARGET -- \
  "nix-env --profile /nix/var/nix/profiles/system --set $PROFILE_PATH && \
   /nix/var/nix/profiles/system/bin/switch-to-configuration switch"
