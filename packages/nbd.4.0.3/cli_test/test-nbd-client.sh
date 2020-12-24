#!/bin/sh

set -eux

CLI=$1

SCRATCH=$(mktemp -d)
EXPORT=$SCRATCH/test

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

# Try listing the exports
# This will fail if the server does not implement NBD_OPT_LIST correctly
nbd-client -l 0.0.0.0
