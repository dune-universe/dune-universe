#!/bin/bash

set -e
# set -u
set -x

if [ -z $1 ]; then
    echo "Usage: $0 <bisect-coverage.json>"
    exit 1;
fi

CI_COMMIT_AUTHOR_NAME=$(echo $CI_COMMIT_AUTHOR  | sed 's/ <.*//')
CI_COMMIT_AUTHOR_EMAIL=$(echo $CI_COMMIT_AUTHOR  | sed 's/.*<\(.*\)>.*/\1/')

export RUN_AT=$CI_JOB_STARTED_AT
export SERVICE_NAME="gitlab-ci"
export SERVICE_NUMBER=$CI_PIPELINE_IID
export SERVICE_JOB_ID=$CI_JOB_ID
export PULL_REQUEST_ID=$CI_OPEN_MERGE_REQUESTS
export COMMIT_SHA=$CI_COMMIT_SHA
export AUTHOR_NAME=$CI_COMMIT_AUTHOR_NAME
export AUTHOR_EMAIL=$CI_COMMIT_AUTHOR_EMAIL
export COMMITER_MAIL=$CI_COMMIT_AUTHOR_NAME
export COMMITER_EMAIL=$CI_COMMIT_AUTHOR_EMAIL
export COMMITER_MESSAGE=$(echo $CI_COMMIT_MESSAGE | sed -z 's@\n@\\n@g')
export BRANCH=$CI_COMMIT_BRANCH

./_ci/coverage_metadata.sh $1
