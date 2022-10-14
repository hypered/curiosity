if [[ -z $NGINX_CONF ]]; then
    scriptdir=$(realpath -s "$0" | xargs dirname)
    echo "Missing static files."
    echo "Did you read $scriptdir/readme.md ?"
    exit 1
fi
rundir=$(mktemp -d -t "curiosity-nginx.XXXXXXXXXX")
echo "[+] Nginx runtime dir: $rundir"
trap 'rm -rf -- "$rundir"' EXIT
cd "$rundir"
cp "$NGINX_CONF" nginx.conf
mkdir -p nginx-cache
sed -i "s~@tmpdir@~$rundir/nginx-cache~g" nginx.conf
echo "[+] Starting nginx on port 8888"
echo "[+] Home page: http://127.0.0.1:8888"
nginx -c "$rundir"/nginx.conf
