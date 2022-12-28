 #! /usr/bin/env bash

# Serve the _site/ directory with browser-sync.

cd _site/

echo "Visit http://127.0.0.1:3000/documentation.html"
echo "Note that a link to e.g. /about should be /about.html"
nix-shell -p nodePackages.browser-sync \
  --run 'browser-sync start --server --files "*.html" --watch --no-open'
