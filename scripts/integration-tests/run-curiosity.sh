if ! command -v cty; then
    scriptdir=$(realpath -s "$0" | xargs dirname)
    echo "Missing cty."
    echo "Did you read $scriptdir/readme.md ?"
    exit 1
fi

echo "[+] Setting up the curiosity runtime env"
rundir=$(mktemp -d -t "curiosity.XXXXXXXXXX")
echo "[+] Curiosity runtime dir: $rundir"
trap 'rm -rf -- "$rundir"' EXIT
cd "$rundir"

# Init curiosity env
cty init

# Start server
echo "[+] Starting the curiosity server"
cty serve --server-port 9100
