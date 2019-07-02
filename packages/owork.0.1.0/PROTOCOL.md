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

### Start

Start the timer.
```
set/start
```

### Stop

Stop the timer.
```
set/stop
```

### Toggle

Toggle the start/stop state of the timer.
```
set/toggle
```

### Reset

Reset the sessions, going back to the `IDLE` state.
```
set/toggle
```

### Restart

Restart the current session.
```
set/restart
```

### Skip

Skip the current session.
```
set/skip
```

## Get requests

### Time

Get the time remaining in the current session.
```
get/time
```

### State

Get the current state.
```
get/state
```

### Completed

Get the number of work sessions completed.
```
get/completed
```

### Paused

Get whether the timer is paused or not.
```
get/paused
```

### Percentage

Get the percentage of the session remaining.
```
get/percentage
```
