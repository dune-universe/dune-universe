#!/bin/sh

set -e
set -x

if [ -z $1 ]; then
    echo "Usage: $0 <bisect-coverage.json>"
    exit 1;
fi

if [ -z ${COVERALLS_REPO_TOKEN}]; then
    echo "Set the variable COVERALLS_REPO_TOKEN"
    exit 1;
fi

metadata_tmp=$(mktemp /tmp/coveralls-metadata.XXXXXXXX.json)
cat <<EOT > $metadata_tmp
{
    "service_name": "$SERVICE_NAME",
    "service_number": "$SERVICE_NUMBER",
    "service_job_id": "$SERVICE_JOB_ID",
    "service_pull_request": "$PULL_REQUEST_ID",
    "repo_token": "$COVERALLS_REPO_TOKEN",
    "git":
    {"head":
    {"id":"$COMMIT_SHA",
    "author_name":"$AUTHOR_NAME",
    "author_email":"$AUTHOR_EMAIL",
    "committer_name":"$COMMITER_MAIL",
    "committer_email":"$COMMITER_EMAIL",
    "message":"$COMMITER_MESSAGE"
    },"branch":"$BRANCH",
    "remotes":{}},
    "parallel": false,
    "flag_name": "test",
    "commit_sha": "$COMMIT_SHA",
    "run_at": "$RUN_AT"
}
EOT

echo "Generated metadata: " $metadata_tmp > /dev/stderr

jq -r -s '.[0] * .[1]' $metadata_tmp $1
