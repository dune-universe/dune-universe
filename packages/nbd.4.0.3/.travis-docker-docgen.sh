#!/bin/bash +x

# SUMMARY:
# Runs .docgen.sh in the Docker container started by .travis-docker.sh in
# addition to the usual tests run by that script, to upload documentation to
# GitHub pages, taking care to avoid exposing $GH_TOKEN and to pass the
# necessary environment variables into the container.

# Make sure we're not echoing any sensitive data
set +x

set -e
set -o pipefail

# Make sure .travis-docker.sh is not echoing any sensitive data
if [ -z "$GH_TOKEN" ]; then
  bash +x -e ./.travis-docker.sh
else
  POST_INSTALL_HOOK="env TRAVIS=$TRAVIS TRAVIS_PULL_REQUEST=$TRAVIS_PULL_REQUEST TRAVIS_BRANCH=$TRAVIS_BRANCH TRAVIS_BUILD_NUMBER=$TRAVIS_BUILD_NUMBER GH_TOKEN=$GH_TOKEN make gh-pages" bash +x -e ./.travis-docker.sh |& sed -e "s/$GH_TOKEN/!REDACTED!/g"
fi
