#!/bin/sh

# This test checks that misbehaving clients sending random data cannot cause
# the server to crash.

set -eux

CLI=$1

SCRATCH=$(mktemp -d)
EXPORT=$SCRATCH/test
DATA=$SCRATCH/data

# Create the file we'll serve
truncate --size=10M "$EXPORT"

# Serve it
$CLI serve --exportname test --no-tls "$EXPORT" &
SERVER=$!
# Wait for the server to start the main loop
sleep 0.1

stop_server() {
  kill -9 $SERVER
}
trap stop_server EXIT

# Keep sending random data to the server
# This loop tests the option handling of the server
for i in $(seq 100)
do
  echo "**** Option handling: iteration $i ****"
  dd if=/dev/urandom bs=1M count=20 | nc 0.0.0.0 10809 || true
  # Check if the server is still running
  stat /proc/$SERVER
done

# This loop tests the command handling of the server
for i in $(seq 100)
do
  echo "**** Command handling: iteration $i ****"
  rm -f "$DATA"
  # We connect to the export and then enter the transmission phase
  printf '\000\000\000\001IHAVEOPT\000\000\000\001\000\000\000\004test' > "$DATA"
  dd if=/dev/urandom bs=1M count=20 >> "$DATA"
  nc 0.0.0.0 10809 < "$DATA" || true
  # Check if the server is still running
  stat /proc/$SERVER
done
