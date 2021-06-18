File Structure for Auditing KBB2017
-----------------------------------

*Security Engineers and Auditors: Everything that says "Unmodified" is a verbatim copy of the Prosecco source code. The git commit and authenticated timestamps are available in <diskuv-communicator-models>/dirsp-exchange/dirsp_exchange.mli*.

``kobeissi_bhargavan_blanchet.ml``
    **Auto-generated**. This is a ProScript-to-OCaml translation of ``proscript-messaging/ps/sp.js`` (aka KBB2017).
    Generated from the dirsp-ps2ocaml.exe program, with auto-formatting by ocamlformat.

``kobeissi_bhargavan_blanchet_intf.ml``
    **New content**. OCaml interface for ``proscript-messaging/ps/sp.js``, plus a few shims for functions that could
    not be machine translated.

``proscript-messaging/*.rst``
    **New content**. The documentation you are reading now.

``proscript-messaging/pscl/pscl.js``
    **Unmodified**. Javascript implementation of the ProScript Cryptography Library. This has implementations
    of SHA256 and other core security primitives.

``proscript-messaging/pscl/dirsp_Dirsp_proscript_intf.ml``
    **New content**. OCaml interface for the ProScript runtime. This is an OCaml reimagining of the
    Javascript API implicit in ``pscl.js``.

``proscript-messaging/pscl/dirsp_proscript.mli``
    **New content**. Just includes the ``Dirsp_proscript_intf.ml``. This is a way to share the same interfaces
    for external use (you!) and internal library use.

``proscript-messaging/pscl/dirsp_proscript.ml``
    **New content**. A hand-written OCaml implementation of the ProScript runtime. It mimics ``pscl.js``.

``proscript-messaging/ps/sp.js``
    **Unmodified**. ProScript implementation of what the Prosecco team calls the version 3 of the
    "Signal Protocol" (X3DH and Double Ratchet). We call it KBB2017.

``proscript-messaging/ps2pv/LICENSE``
    **Unmodified**. License file from the proscript-messaging team

``proscript-messaging/ps2pv/globals.ml``
    **Unmodified**. Global variables and common utility functions.

``proscript-messaging/ps2pv/lexer.ml``
    **Unmodified**. Lexical instructions for how to create tokens (ex. ``BOOL``, ``FUNCTION``, ``FINALLY``, ``IF``) from the ProScript dialect of JavaScript.

``proscript-messaging/ps2pv/ast.ml``
    **Unmodified**. The abstract syntax tree of ProScript. The tree contains the relevant details of ProScript that are necessary
    for recreating a proof with the author's ProVerif tool; it is **not** a perfect representation of ProScript. So we need
    to replace the lossiness (like losing arrays of hexadecimal constants).

``proscript-messaging/ps2pv/parser.mly``
    **Unmodified**. Instructions for how to produce the AST from the lexer.

``proscript-messaging/ps2pv/pretty.ml``
    **Unmodified**. Pretty-printer of the AST.

``proscript-messaging/ps2pv/_tags``
    **Unmodified**. Build instructions for the legacy ``ocamlbuild`` tool. It allows for building a legacy syntax and parser
    generators for OCaml called ``camlp4``, which is difficult to build with modern OCaml tooling.

``proscript-messaging/ps2pv/lexerror.ml``
    **New content**. Error utility functions to report lexical information (line and column) when there is an error.
    Similar to the Prosecco team's ``error.ml`` file, which is not included in this distribution.

``proscript-messaging/ps2pv/ast2ocaml.ml`` ``proscript-messaging/ps2pv/astpredicates.ml``
    **New content**. Translates the AST into OCaml source code.
    Similar to the Prosecco team's ``pretty.ml`` file; both of which walk the AST.

``proscript-messaging/ps2pv/ast2ocaml_tests.ml``
    **New content**. Tests for ``ast2ocaml.ml`` (ex. overflow/underflow compliance).

``proscript-messaging/ps2pv/dirsp_ps2ocamlcore.ml``
    **New content**. Parses source code into AST and translates via ``ast2ocaml.ml`` into OCaml.

``proscript-messaging/ps2pv/dirsp_ps2ocaml.ml``
    **New content**. A command-line driver. When you run ``dirsp-ps2ocaml.exe`` you are running this.

``proscript-messaging/.gitignore``
    **New content**. Which files are ignored for git

``proscript-messaging/.ocamlformat``, ``proscript-messaging/.ocamlformat-ignore``
    **New content**. Formatting instructions for new content.

``proscript-messaging/debug-parsing.mlt``
    **New content**. Instructions for ``utop`` to quickly begin a debug session.


