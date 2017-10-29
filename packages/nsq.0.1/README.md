# NSQ Client Library in OCaml

A simple client library for the [NSQ](http://nsq.io) message platform.

See examples for a simple example that publishes and subscribes on the same topic.

## Getting started

To compile the example program:

*NOTE*: You will need to change the IP address to that of the docker host in example.ml

```
opam install containers lwt ocplib-endian integers
make
```

Spin up NSQD using docker:
```
docker run nsqio/nsq nsqd
```

Run the example:

```
_build/default/examples/example.exe
```
