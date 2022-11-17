#!/usr/bin/env bash
set -eu

if [[ -z "${CURIOSITY_ENDPOINT:-}" ]]; then
    scriptdir=$(realpath -s "$0" | xargs dirname)
    echo "Missing curiosity endpoint."
    echo "Did you read $scriptdir/readme.md ?"
    exit 1
fi

echo "[+] Querying curiosity root endpoint"
curl -s --show-error -f -v "$CURIOSITY_ENDPOINT" > /dev/null
echo "[+] Checking that static assets are brotli-compressed"
curl -s --show-error -H "Accept-Encoding: br" "$CURIOSITY_ENDPOINT"/static/css/main.css | brotli -d | grep "@charset"
echo "[+] Checking that SSI is functional"
curl -s --show-error "$CURIOSITY_ENDPOINT"/documentation/scenarios | grep 'href="/partials/scenarios/'
