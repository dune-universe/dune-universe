*sphinxcontrib-ocaml* is a `Sphinx <http://www.sphinx-doc.org/>`_ (1.6.3+) extension to document `OCaml <https://ocaml.org/>`_ libraries.
It provides a Sphinx `domain <http://www.sphinx-doc.org/en/stable/domains.html>`_ for OCaml and
`autodoc <http://www.sphinx-doc.org/en/stable/ext/autodoc.html>`_-like directives to generate documentation from source code.

It's licensed under the `MIT license <http://choosealicense.com/licenses/mit/>`_.
It's available in two parts, on the `Python package index <http://pypi.python.org/pypi/sphinxcontrib-ocaml>`_
and `OPAM <https://opam.ocaml.org/packages/sphinxcontrib-ocaml/>`_ (both are required).
Its `documentation <http://jacquev6.github.io/sphinxcontrib-ocaml>`_
and its `source code <https://github.com/jacquev6/sphinxcontrib-ocaml>`_ are on GitHub.

Questions? Remarks? Bugs? Want to contribute? `Open an issue <https://github.com/jacquev6/sphinxcontrib-ocaml/issues>`__!

.. image:: https://img.shields.io/travis/jacquev6/sphinxcontrib-ocaml/master.svg
    :target: https://travis-ci.org/jacquev6/sphinxcontrib-ocaml

.. image:: https://img.shields.io/github/issues/jacquev6/sphinxcontrib-ocaml.svg
    :target: https://github.com/jacquev6/sphinxcontrib-ocaml/issues

.. image:: https://img.shields.io/github/forks/jacquev6/sphinxcontrib-ocaml.svg
    :target: https://github.com/jacquev6/sphinxcontrib-ocaml/network

.. image:: https://img.shields.io/github/stars/jacquev6/sphinxcontrib-ocaml.svg
    :target: https://github.com/jacquev6/sphinxcontrib-ocaml/stargazers

Status
======

sphinxcontrib-ocaml is still highly experimental.
Interfaces may be changed unannounced.
We welcome all feedback from our daring early users.

Quick start
===========

Install both packages::

    $ pip3 install sphinxcontrib-ocaml
    $ opam install sphinxcontrib-ocaml

Enable and configure the Sphinx extension in your ``conf.py`` file::

    extensions.append("ocaml")
    primary_domain = "ocaml"  # Optional
    ocaml_source_directories = ["src"]
    ocaml_findlib_packages = ["batteries", "js_of_ocaml"]

And document your module (in an ``.rst`` file)::

    .. autoocamlmodule:: MyModule
