#!/bin/sh

export GOMAXPROCS=1

exec wrk2 -t8 -c10000 -d120S --timeout 2000 -R ${1:-60000} --latency http://127.0.0.1:${2:-8080}/${3:-''}
