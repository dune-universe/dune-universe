#!/usr/bin/sh

cat `sed 's|^ref: |\.\./\.git/|' <../.git/HEAD` \
    | sed 's|\(.*\)|let sha1 = "\1"|'          \
    > sha1.ml
