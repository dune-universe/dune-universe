.. _autodoc:

==================
Autodoc directives
==================

Directives
==========

.. highlight:: rst

.. rst:directive:: autoocamlmodule Name

    Automatically document a module.
    The ``.mli`` file must be in a directory listed in `data:`ocaml_source_directories`.
    ``.cmi`` files required during parsing and typing must be available from :data:`ocaml_findlib_packages`
    or :data:`ocaml_include_directories`.

Source code attributes
======================

Documentation
-------------

::

    (** Double-starred comments *)

    [@@ocaml.doc]

Special behavior
----------------

::

    [@autodoc.inline]

    [@autodoc.hide]
