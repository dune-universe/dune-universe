#!/bin/sh

set -e

database="$1"
shift
cmd="$@"

# checking if database folder exists instead of connecting to mysql which would require to install mysql client in container
until [ -d /workspace/.devcontainer/data/db/$database ]; do
  echo "MySQL database not yet created - sleeping"
  sleep 1
done

echo "MySQL database created - executing command"
exec $cmd
