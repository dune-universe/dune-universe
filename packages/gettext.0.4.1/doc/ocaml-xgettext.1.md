% OCAML-XGETTEXT(1)
% Sylvain Le Gall
% 2008-04-29

# NAME
ocaml-xgettext - program to extract translatable strings from OCaml source file.

# SYNOPSIS
ocaml-xgettext [ppx arguments] [filename]

# DESCRIPTION
This manual page documents briefly the ocaml-xgettext

ocaml-xgettext is a ppx filter. It outputs an OCaml marshalled data structure
that can only be understood by ocaml-gettext. The purpose of this program is to
be a backend for OCaml source code string extraction. You should not use it
directly.

# SEE ALSO
`ocaml-gettext`(1)
