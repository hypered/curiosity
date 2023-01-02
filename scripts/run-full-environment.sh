#!/usr/bin/env bash

$(nix-build --no-out-link -A runenv)/bin/run-full-environment
