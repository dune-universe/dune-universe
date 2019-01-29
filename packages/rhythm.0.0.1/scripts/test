#!/bin/bash
set -e

# This will be the location of this script.
DIR="$(cd "$(dirname "$0")" && pwd)"

# Run TestsExe.exe with correct root set.
RHYTHM_PROJECT_ROOT="$DIR/../" esy x "TestsExe.exe" "$@"
