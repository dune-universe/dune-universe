# Protocol

This document outlines the expected requests and responses of the server.

## Format of requests

Requests are separated into `set` and `get` type, they are also composed of an associated action corresponding to the type. They have the following format:
```
<type>/<action>
```

E.g.:
```
set/skip
get/time
```

## Set requests

| Query | Detail |
|---|---|
| `set/start` | Start the timer |
| `set/stop` | Stop the timer |
| `set/toggle` | Toggle the start/stop state of the timer |
| `set/reset` | Reset the sessions, going back to the `IDLE` state |
| `set/restart` | Restart the current session|
| `set/skip` | Skip the current session |

## Get requests

| Query | Detail |
|---|---|
| `get/time` | Get the time remaining in the current session |
| `get/state` | Get the current state |
| `get/completed` | Get the number of work sessions completed |
| `get/paused` | Get whether the timer is paused or not |
| `get/percentage` | Get the percentage of the session remaining |
