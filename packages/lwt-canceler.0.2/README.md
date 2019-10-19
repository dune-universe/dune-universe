# Lwt-canceler

Primitives for synchronization of cancellations.

A `Canceler.t` is a three-states synchronization object with transitions
*waiting* → *canceling* → *canceled*, starting in waiting state. Hooks can be
attached to the canceler. Hooks are triggered when switching to the canceling
state. The canceler switches to canceled state when the hooks have completed.
