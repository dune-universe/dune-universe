#!/bin/bash

./_build/default/osbx.exe encode -f dummy dummy1.sbx --sbx_version 1

./_build/default/osbx.exe encode -f dummy dummy2.sbx --sbx_version 2

./_build/default/osbx.exe encode -f --nometa dummy dummy3.sbx --sbx_version 3

./_build/default/osbx.exe decode -f dummy1.sbx dummy1

./_build/default/osbx.exe decode -f dummy2.sbx dummy2

./_build/default/osbx.exe decode -f dummy3.sbx dummy3
