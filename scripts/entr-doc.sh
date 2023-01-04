#! /usr/bin/env nix-shell
#! nix-shell -i bash -p entr gnumake pandoc mandoc stork"

# Render .md files to .html as soon as they are modified.

mkdir -p _site/static
STATIC_DIR=$(nix-build -A static --no-out-link)
cp -r $STATIC_DIR/{css,favicon.ico,fonts,images,js} _site/static/
make -f scripts/doc.Makefile entr
