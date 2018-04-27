#!/bin/bash

set -o errexit

if [ -d /tmp/node_modules_for_JsOfOCairo ]
then
    rm -rf node_modules
    cp -R /tmp/node_modules_for_JsOfOCairo node_modules
else
    npm install "$@"
    rm -rf /tmp/node_modules_for_JsOfOCairo
    cp -R node_modules /tmp/node_modules_for_JsOfOCairo
fi
