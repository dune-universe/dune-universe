#!/usr/bin/env bash

# this script gets called by the owork server with the new state
case "$1" in
"Idle")
    text="Idle"
    ;;
"Work")
    text="Start work"
    ;;
"Short")
    text="Take a short break"
    ;;
"Long")
    text="Take a long break"
    ;;
esac

notify-send "Owork" "$text"
