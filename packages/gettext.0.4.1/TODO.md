v 0.4.0 :
- Better parse comments, to keep licence et al 
- Try to be faster than gettext C library
- Try to get GettextStub.* and GettextDummy.* be typed GettextTypes.REALIZE_TYPE (trying to avoid
  the use of a .mli file).
- Reread the code to improve general layout (naming scheme, exception name... ie better style)
- Correct the BUG: related to bug from ocaml-fileutils
- Intercept problem when recoding string (charset), errors should be wrap inside a failsafe or
  at least raise a coherent exception.
- Create what is necessary for running into Threaded env
- Write a patch for xgettext et al
