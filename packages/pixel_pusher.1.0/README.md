Pixel Pusher library for OCaml
===========

Pixel Pushers are these devices that you can plug Ethernet and LED strips into.

You can send UDP packets to the devices over Ethernet to tell them to set the
colors of the LED strips.

The normal way to drive these is through something called Processing, a Java-like
environment that includes a Pixel Pusher plugin.

If you would rather use OCaml to drive them, this library is for you!

Installing
---

`opam install pixel_pusher`

Using
---

Add `pixel_pusher` to your dune config libraries stanza.

You can look at the
https://github.com/mbacarella/kindredspirit/kindredspirit.ml project for an
example of real-world usage.

Bugs
---

Feel free to file a bug report if you encounter any issues.  Shoot me an email
if you have any questions: michael.bacarella@gmail.com.

Happy hacking!
