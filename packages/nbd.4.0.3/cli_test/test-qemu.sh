#!/bin/sh

set -eux

CLI=$1

SCRATCH=$(mktemp -d)
EXPORT=$SCRATCH/test
OUTPUT=$SCRATCH/out

# Create a test file
dd if=/dev/urandom of="$EXPORT" bs=1M count=40

# Serve it
$CLI serve --exportname test --no-tls "$EXPORT" &
SERVER=$!
# Wait for the server to start the main loop
sleep 0.1

stop_server() {
  kill -9 $SERVER
}
trap stop_server EXIT

# Download it as raw from the server
qemu-img convert 'nbd:0.0.0.0:10809:exportname=test' -O raw "$OUTPUT"

# Check that the two files are the same
cmp --silent "$EXPORT" "$OUTPUT"
