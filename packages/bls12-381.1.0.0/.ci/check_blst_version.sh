COMMIT="095a8c53787d6c91b725152ebfbbf33acf05a931"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd -P)"
TMP_DIRECTORY=$(mktemp -d)
cd ${TMP_DIRECTORY}
wget https://github.com/supranational/blst/archive/${COMMIT}.zip
unzip ${COMMIT}.zip
cd blst-${COMMIT}
diff -qr . $SCRIPT_DIR/../src/blst/libblst
