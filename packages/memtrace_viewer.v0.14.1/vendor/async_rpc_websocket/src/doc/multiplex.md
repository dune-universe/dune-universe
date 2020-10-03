# How not to multiplex RPC

# Goal

- Serve RPCs via multiple protocols (GET/websocket, raw RPC, and Kerberos) on the same
  port
- Authentication support (Kerberos or web-based, depending on protocol)

# Method 1

## Observation

When handling a websocket (`GET`) request, there are several layers of protocol stuff, but
in the end you get a `Reader.t` and `Writer.t` that can be used as a transport for an RPC
server. When serving RPCs directly, the reader and writer from the socket are the
transport. For Kerberos, there is a `Kerberized_tcp` module that can be used to create a
server that also eventually produces a reader and writer.

## Proposed solution

Write a multiplexer that determines which protocol a request is using and pass the
original reader and writer through the appropriate handlers, which return a reader and
writer for use as an RPC transport.

## Problems

A Kerberized RPC is not equivalent to a normal RPC served on a Kerberized TCP server. Most
notably, the `Kerberized_rpc` inserts additional implementations into the RPC
server/client and does some negotiation using RPCs, beyond the authentication that already
took place at the level of the Kerberized TCP socket. This means that the RPC server can't
be created independently from the protocol handlers.

Different protocols extract different information during their handshake, e.g.
authentication details. Users of the multiplexer would need to specify what they want to
do with this extra information for each protocol, so they aren't completely insulated from
the differences between protocols.

# Method 2

## Observation

Even though there is not much sharable logic in the handlers for the different protocols,
determining which protocol a request is using is relatively easy, and we have existing
code for each protocol that serves RPCs.

## Proposed solution

Write a multiplexer that determines which protocol a request is using and forwards it to
the RPC server that handles those types of requests. Besides the multiplexer, the only
shared code is in the RPC implementations and `initial_connection_state` function.

## Problems

If someone wants to add a new protocol, it isn't enough to add request recognition and
handshake code. This solution requires separate handlers for each protocol. Similarly, if
someone wants to use the multiplexer for something other than an RPC server, they must
reimplement all the handshake logic and other boilerplate for each protocol type; none of
that is encapsulated in the multiplexer itself.

Both `Cohttp` (for websocket handling) and `Krb` (for Kerberos) create their own TCP
socket, whereas we would like them to expose a reader/writer handler that can be called
when a request using that protocol is recieved. This can probably be solved by refactoring
those modules to expose an intermediate step.

Since protocols don't share a single transport on which RPCs are ultimately served,
multiple instances of RPC servers are created. This may not be a big deal.

# Method 3

## Observation

Specialization makes things easier. We only care about handling RPCs, and we only care
about our three protocols.

## Proposed solution

The multiplexer turns into a protocol identifier, which peeks at the first few bytes of
a reader for a request and returns the protocol, which is just a variant. We can just
ignore the version information that `Protocol_version_header` extracts from the header.
After a request is redirected to a specific handler, that handler can extract whatever
information it requires.
Modifying `Protocol_version_header` is probably the correct thing to do here; in addition
to the magic-number-based identification it currently does, it needs to be able to
identify the websocket protocol based on the three-character prefix.

The interface we want to provide to end users is similar to
`Kerberized_tcp.Server.create`, or `Kerberized_rpc.Connection.serve{_with_anon,}`.
We can clone their interfaces and add additional optional arguments for the websocket
component. Once we have something that works, we should be able to push the changes back
into `Krb`.

We will need to modify `Krb` and `Cohttp` to expose a reader/writer handler rather than
always creating their own TCP servers, as in Method 2.
