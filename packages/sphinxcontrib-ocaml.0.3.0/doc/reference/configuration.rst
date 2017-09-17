====================
Configuration values
====================

The following settings are available in your ``conf.py`` file.


.. data:: ocaml_autodoc_executable

    The name of, or the full path to, the `sphinxcontrib-ocaml-autodoc` executable.
    The default value (``"sphinxcontrib-ocaml-autodoc"``) matches what is installed by
    the `OPAM package <https://opam.ocaml.org/packages/sphinxcontrib-ocaml/>`_, so it should be fine.
    Use this setting if you're doing something special with your OPAM root.

.. data:: ocaml_source_directories

    The list of directories to consider when looking for an ``.mli`` file in autodoc directives.
    You *must* set this if you use :ref:`autodoc directives <autodoc>`.

.. data:: ocaml_findlib_packages

    The list of `findlib <http://projects.camlcity.org/projects/findlib.html>`_ packages to use
    when parsing an ``.mli`` file in autodoc directives.

.. data:: ocaml_include_directories

    The list of directories (containing ``.cmi`` files) to include when parsing an ``.mli`` file in autodoc directives.
    You should use :data:`ocaml_findlib_packages` when possible.
