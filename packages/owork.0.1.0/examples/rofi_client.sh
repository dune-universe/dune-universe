#!/usr/bin/env bash

# if script is run when rofi is in use then kill it and exit
pgrep rofi && pkill rofi && exit 0

# inputs to be used for rofi
inputs=(
    "Toggle pause"
    "Reset timer"
    "Restart session"
    "Skip session"
)

# if the owork server is not yet started then start it
pgrep owork &>/dev/null || owork &

# send the given data to the server at the appropriate default socket
send_to_server() {
    echo "$1" | nc -U /tmp/owork.sock
}

# ask the user to select one of the above options
selection=$(printf "%s\n" "${inputs[@]}" | rofi -dmenu -p "Timer" -i)

# ask the server to perform the corresponding action to the user's choice
case $selection in
"Toggle pause")
    send_to_server "set/toggle"
    ;;
"Reset timer")
    send_to_server "set/reset"
    ;;
"Restart session")
    send_to_server "set/restart"
    ;;
"Skip session")
    send_to_server "set/skip"
    ;;
esac
