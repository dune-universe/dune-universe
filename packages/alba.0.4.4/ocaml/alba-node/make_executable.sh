#!/bin/sh

(echo '#!/usr/bin/env node'; cat $1) > $2

chmod u+x $2
