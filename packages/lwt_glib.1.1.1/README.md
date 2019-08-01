# Lwt_glib: GLib event loop for Lwt

A GLib/[GTK][gtk] event loop that plugs into [Lwt's][lwt]
[`Lwt_engine`][lwt_engine], so you can use Lwt in your GTK OCaml app.

To install, do `opam install lwt_glib`.

For documentation, see the [`.mli` file][mli].

This package was formerly maintained in the [main Lwt repo][lwt]. Most of the
git history and changelog still refer to Lwt_glib's days in Lwt.

[gtk]: https://www.gtk.org/
[lwt]: https://github.com/ocsigen/lwt
[lwt_engine]: https://ocsigen.org/lwt/3.1.0/api/Lwt_engine
[mli]: https://github.com/aantron/lwt_glib/blob/master/src/lwt_glib.mli
