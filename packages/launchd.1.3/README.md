OCaml bindings to the launchd API
=================================

This library is useful if you wish to make a service which is
automatically started by launchd.

Please read
[the Unix API documentation](https://mirage.github.io/ocaml-launchd/launchd) (and
[the Lwt_unix version](https://mirage.github.io/ocaml-launchd/launchd.lwt)).

Example
-------

First build the code

```sh
make
```

Assuming you are running under OS X, install the example services
as LaunchAgents for the current user:

```sh
$ ./install-example.sh
```

Check the agents have registered:

```sh
$ launchctl list | grep org.recoil
34787	0	org.recoil.dave.anotherd
-	0	org.recoil.dave.exampled
```

One of them is listening on port 8081:

```sh
$ telnet localhost 8081
Trying ::1...
telnet: connect to address ::1: Connection refused
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Hello there!
Connection closed by foreign host.
```

And the other is listening on a Unix domain socket:

```sh
$ ls -l /var/run/org.recoil*
srw-rw-rw-  1 djs  staff  0  9 Oct 14:06 /var/run/org.recoil.dave.exampled
```

Run this script to remove the agents:

```sh
$ ./uninstall-example.sh
```
