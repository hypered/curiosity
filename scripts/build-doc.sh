#! /usr/bin/env nix-shell
#! nix-shell -i bash -p entr gnumake mandoc pandoc stork

make -f scripts/doc.Makefile
