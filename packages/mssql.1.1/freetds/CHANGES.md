Dev
---

- Port to Dune and `dune.configurator`.

0.6 2018-05-29
--------------

- Add a parameter `version` to `Dblib.connect` to be able to choose
  the protocol version.  Explain how the old protocol versions
  do not allow empty strings which will be returned as `' '`.
- Keep the compatibility with FreeTDS version < 0.91.
- Keep int64 as such on the OCaml side (they used to be transformed
  into strings).
- Document `Dblib.severity`.
- Make error messages more informative by splicing the binding on a
  finer level.  A configure script ensures the synchronization with
  values defined in the C header files.
- Add tests for `Dblib` and `Ct` (checked by AppVeyor on
  Windows/mingw).
- Improve the interface of `Ct.get_messages`.
- Complete the depexts list.

0.5.2 2018-04-15
----------------

- Better error messages (commits #94134f7 and #bdc2c99)
- Port to Jbuilder/Dune (Brendan Long) and topkg
- Add basic unit tests (Brendan Long)
- Fix all build warnings
- Improve Travis CI scripts
