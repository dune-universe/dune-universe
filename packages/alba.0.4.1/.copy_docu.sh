#!/bin/sh

echo "Copy documentation on travis-ci"

if [ "$TRAVIS" = "true" ] ; then
    echo "We are on travis-ci, copy documentation"
    cp -r _build /tmp
    ls -la /tmp
fi

