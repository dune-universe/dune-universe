#!/bin/bash

# Run this script to benchmark the NBD server using qemu-img.

set -eux

dd if=/dev/zero of=/tmp/test bs=1M count=100
_build/default/cli/main.exe serve --exportname test --no-tls /tmp/test &
SERVER_PROCESS=$!
# Wait for the server to start the main loop
sleep 0.1

stop_server() {
  kill -9 $SERVER_PROCESS
}
trap stop_server EXIT

qemu-img bench 'nbd:0.0.0.0:10809:exportname=test'
