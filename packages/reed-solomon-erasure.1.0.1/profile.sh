#!/bin/bash

jbuilder exec benchmark/bench.exe

gprof _build/default/benchmark/bench.exe gmon.out > profile.txt
