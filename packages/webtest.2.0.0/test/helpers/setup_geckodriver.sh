#!/bin/bash

DRIVER_VERSION=v0.26.0
DRIVER_FILE=geckodriver-${DRIVER_VERSION}-linux64.tar.gz

cd /tmp

wget https://github.com/mozilla/geckodriver/releases/download/${DRIVER_VERSION}/${DRIVER_FILE}
tar -zxf ${DRIVER_FILE}
sudo mv geckodriver /usr/local/bin/geckodriver
