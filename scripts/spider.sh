#! /usr/bin/env bash

# Find broken links on cty.hypered.systems. This doesn't check for outbound links.
# Once done, check the bottom of the generated spider.log file (or lines
# preceding the "broken link" markers). Without the --wait, this takes about
# 11 seconds. With --wait 1, this takes about 6 minutes.

wget \
  --spider \
  --no-directories \
  --no-verbose \
  --recursive \
  --execute robots=off \
  --level 0 \
  --wait 1 \
  --output-file spider.log \
  https://cty.hypered.systems
