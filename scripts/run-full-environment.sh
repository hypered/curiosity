#!/usr/bin/env bash
scriptdir=$(dirname $0)
$(nix-build "$scriptdir"/integration-tests --no-out-link -A run-full-environment)/bin/run-full-environment
