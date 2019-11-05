#!/usr/bin/env bash

# this script gathers data from the server and outputs some text corresponding to the values

socket=/tmp/owork.sock

# send the given data to the server and get response
send_to_server() {
    [ -e $socket ] && echo "$1" | nc -U $socket
}

# get the values
state=$(send_to_server "get/state")
time=$(send_to_server "get/time")
sessions_complete=$(send_to_server "get/completed")
paused=$(send_to_server "get/paused")
percent=$(send_to_server "get/percentage")

# if state was empty then nothing to display
if [ -z "$state" ]; then
    exit 1
fi

# generate the output text
if [ $paused == "true" ]; then
    if [ $state == "Idle" ]; then
        text=" $state"
    else
        text=" $state $time $sessions_complete"
    fi
else
    text="$state $time $sessions_complete"
    if [ $percent -ge 60 ]; then
        text=" $text"
    elif [ $percent -ge 20 ]; then
        text=" $text"
    else
        text=" $text"
    fi
fi

echo $text
