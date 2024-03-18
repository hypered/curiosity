#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

# Use hlint for linting, and apply suggestions using the refactor program from
# the apply-refact package.

PROCS="$(nproc)"

find bin/ src/ tests/ -name "*.hs" | \
  xargs -P $PROCS -I {} hlint --refactor-options="-i" --refactor {}
