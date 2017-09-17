============
Both domains
============

This proves that we can document both Python and OCaml elements in the same document.

.. default-domain:: nope

Definitions
-----------

.. ocaml:module:: M

    This is an OCaml module named `M`.

    .. ocaml:module:: S

        This is an OCaml module named `S` in module `M`.

        .. ocaml:val:: v

            This is an OCaml value named `v` in module `M.S`.

.. py:module:: M

This is a Python module named `M`.

.. py:module:: M.S

This is a Python module named `S` in module `M`.

.. py:data:: v

    This is a Python data named `v` in module `M.S`.

References
----------

We can link to modules: :ocaml:mod:`M` and :py:mod:`M`.

We can link to sub-modules: :ocaml:mod:`M.S` and :py:mod:`M.S`.

We can link to fully qualified values: :ocaml:val:`M.S.v` and :py:data:`M.S.v`.

We can link to fully qualified values with explicit titles: :ocaml:val:`vv <M.S.v>` and :py:data:`vv <M.S.v>`.

We can link to fully qualified values and strip their qualifications: :ocaml:val:`~M.S.v` and :py:data:`~M.S.v`.

We can link to non-qualified values: :ocaml:val:`.v` and :py:data:`.v`.

We can link to non-qualified values with explicit titles: :ocaml:val:`vv <.v>` and :py:data:`vv <.v>`.

We can link to partially qualified values: :ocaml:val:`.S.v` and :py:data:`.S.v`.

We can link to partially qualified values with explicit titles: :ocaml:val:`vv <.S.v>` and :py:data:`vv <.S.v>`.

We can link to partially qualified values and strip their qualifications: :ocaml:val:`~.S.v` and :py:data:`~.S.v`.

.. Without a valid default domain, this reference fails: :mod:`M`.

.. default-domain:: ocaml

We can refer to OCaml as default domain: :val:`M.S.v`.

.. default-domain:: py

We can refer to Python as default domain: :data:`M.S.v`.
