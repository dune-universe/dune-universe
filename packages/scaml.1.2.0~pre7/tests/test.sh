#!/bin/bash
set -e

# Disable the disclaimer message of tezos-node
export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y 

# Where am I?
SCRIPT_DIR="$(cd "$(dirname "$0")" && echo "$(pwd -P)")"

# Where to work?
BUILD_DIR=$SCRIPT_DIR/_build
if [ ! -d $BUILD_DIR ]; then mkdir $BUILD_DIR; fi

# Make sure the library module is COMPiled
if [ ! -d $BUILD_DIR ]; then mkdir $BUILD_DIR; fi

dune build ../lib/SCamLib.cmxa
SCAMLIB_DIR=../_build/install/default/lib/scaml/scamlib

# Compilation command
COMP="dune exec ../driver/main.exe -- --scaml-noscamlib --scaml-dump-iml --scaml-time -I $SCAMLIB_DIR"

# Optional: tezos-client
TEZOS_CLIENT=`which tezos-client || true`

# Input <ML>
# Output TZ
function compile () {
    # Compile it under $BUILD_DIR
    if [ ! -d $BUILD_DIR ]; then mkdir $BUILD_DIR; fi
    local ml=$BUILD_DIR/$(basename $1)
    cp $1 $ml
    # Remove old output files
    local iml=`echo $ml | sed -e 's/\.ml$/.iml/'`
    TZ=`echo $ml | sed -e 's/\.ml$/.tz/'`
    rm -f "$iml" "$TZ"

    # Must be rejected?
    local must_reject=$(grep REJECT $ml || true)

    # Compile!
    if [ -z "$must_reject" ]; then
	echo $COMP $ml
	(cd $SCRIPT_DIR; $COMP $ml)
    else
	echo $COMP $ml : THIS COMPILATION MUST FAIL
	if  (cd $SCRIPT_DIR; $COMP $ml)
	then
	    echo "Error: COMPILATION UNEXPECTEDLY SUCCEEEDED"; exit 2
	else
	    echo "Ok: Compilation failed as expected"
	fi
    fi
}

# Input: <code>
# Output: CONVERSION
function convert () {
    echo "converting $1 ..."
    tmp=`mktemp`
    echo "open SCaml" > $tmp
    echo "let x = $1" >> $tmp
    cat $tmp
    CONVERSION=$($COMP -I _build --scaml-convert-value x -impl $tmp | sed -e 's/^x: //')
    echo "converted to $CONVERSION"
}

# Input <ML> <TZ>
# Output: none
function run () {

    local ml=$1
    local tz=$2

    # Must this test fail ?
    local must_fail=$(grep MUST_FAIL $ml || true)

    # STORAGE=.*$
    local storage=$(grep STORAGE= $i || true)
    if [ -z "$storage" ]; then
	storage='Unit'
    else
	storage=`echo "$storage" | sed -e 's/.*STORAGE=//'`
	convert "$storage"
	storage=$CONVERSION
    fi

    # INPUT=.*$
    local input=$(grep INPUT= $i || true)
    if [ -z "$input" ]; then
	input='Unit'
    else
	input=`echo $input | sed -e 's/.*INPUT=//'`
	convert "$input"
	input=$CONVERSION
    fi

    # ENTRY=.*$
    local entry=$(grep ENTRY= $i || true)
    if [ -z "$entry" ]; then
	entry=""
    else
	entry=`echo $entry | sed -e 's/.*ENTRY=//'`
	entry=" --entrypoint $entry "
    fi

    echo Executing $TEZOS_CLIENT typecheck script $tz
    echo INPUT:
    echo "$input"
    
    $TEZOS_CLIENT typecheck script $tz

    # Really weird but --source is to set SENDER and --payer to set SOURCE
    echo "Executing $TEZOS_CLIENT run script $tz on storage $storage and input $input --source bootstrap1 --payer bootstrap2 $entry"

    if [ -z "$must_fail" ]; then
	$TEZOS_CLIENT run script $tz on storage "$storage" and input "$input" --source bootstrap1 --payer bootstrap2 $entry
    else
	echo THIS TEST MUST FAIL
	if
    	    $TEZOS_CLIENT run script $tz on storage "$storage" and input "$input" --source bootstrap1 --payer bootstrap2 $entry
	then
	    echo "Error: TEST UNEXPECTEDLY SUCCEEEDED"; exit 2
	else
	    echo "Ok: Test failed expectedly"
	fi
    fi
}

for i in $*
do

  echo "----- $i"    
  case "$i" in
  *.tz)
      # Do nothing if it is *.tz
      TZ="$i"
      ;;
  *)
      compile "$i"
      ;;
  esac

  # If tz Compilation is successful, and if there is tezos-client in the PATH,
  # let's try to execute it.
  if [ -f "$TZ" -a -n "$TEZOS_CLIENT" ]; then
      run "$i" "$TZ"
  fi
done
