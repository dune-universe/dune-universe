#!/bin/bash

set -e
set -u

if [ -z $1 ]; then
    echo "Usage: $0 <bisect-coverage.json>"
    exit 1;
fi

export RUNAT=$(date +'%Y-%m-%d %H:%m:%S %z')
export SERVICE_NAME="local"
export SERVICE_NUMBER=1000
export SERVICE_JOB_ID=1001
export PULL_REQUEST_ID=1002
export COMMIT_SHA=$(git rev-parse HEAD)
export AUTHOR_NAME=$(git log --oneline --format='%an' -n 1 HEAD)
export AUTHOR_EMAIL=$(git log --oneline --format='%ae' -n 1 HEAD)
export COMMITER_MAIL=$(git log --oneline --format='%cn' -n 1 HEAD)
export COMMITER_EMAIL=$(git log --oneline --format='%ce' -n 1 HEAD)
export COMMITER_MESSAGE=$(git log --oneline --format='%s' -n 1 HEAD)
export BRANCH=$(git rev-parse --abbrev-ref HEAD)

./_ci/coverage_metadata.sh $1
