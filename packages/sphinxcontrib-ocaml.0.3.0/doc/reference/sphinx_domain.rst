===================
OCaml Sphinx domain
===================

.. default-domain:: ocaml

.. highlight:: rst

Remember that these directives and roles are in a Sphinx `domain <http://www.sphinx-doc.org/en/stable/domains.html>`_,
so you can either prefix them with ``ocaml:`` or set the default domain to ``"ocaml"``, either with the ``default-domain`` directive::

    .. default-domain:: ocaml

or in the ``conf.py`` file:

.. code-block:: python

    primary_domain = "ocaml"

Examples below assume the default domain is set.

Directives
==========

Values and types
----------------

.. rst:directive:: .. val:: name

    Document a value::

        .. val:: v

            Some doc for *v*.

    is rendered as:

        .. val:: v
            :noindex:

            Some doc for *v*.

    The value's type can be documented using the ``:type:`` option::

        .. val:: coords
            :type: (int * int) list

            The coordinates.

    ..

        .. val:: coords
            :noindex:
            :type: (int * int) list

            The coordinates.

    Refer to values using the :rst:role:`val` role.

.. rst:directive:: .. type:: name

    Document a type::

        .. type:: t

            Some doc for *t*.

    ..

        .. type:: t
            :noindex:

            Some doc for *t*.

    The type's parameters can be documented using the ``:parameters:`` option::

        .. type:: t3
            :parameters: ('a, 'b, 'c)

            *t3* has three parameters.

    ..

        .. type:: t3
            :noindex:
            :parameters: ('a, 'b, 'c)

            *t3* has three parameters.

    The type's manifest (*i.e.* if it is an alias for some other type) can be documented using the ``:manifest:`` option::

        .. type:: int_list
            :manifest: int list

            A list of integers.

    ..

        .. type:: int_list
            :noindex:
            :manifest: int list

            A list of integers.

    The type's kind (*i.e.* its constructors and record labels) can be documented using the ``:kind:`` option
    and the ``:constructor:`` and ``:label:`` doc fields::

        .. type:: variant
            :kind: A | B of int

            A variant type.

            :constructor A: a

            :constructor B: b

    ..

        .. type:: variant
            :kind: A | B of int

            A variant type.

            :constructor A: a

            :constructor B: b

    ::

        .. type:: record
            :kind: {a: int; b: float}

            A record type.

            :label a: a

            :label b: b

    ..

        .. type:: record
            :noindex:
            :kind: {a: int; b: float}

            A record type.

            :label a: a

            :label b: b

    Refer to types using the :rst:role:`typ` role.

.. rst:directive:: .. exception:: name

    Document an exception::

        .. exception:: MyException

            Some doc for *MyException*.

    ..

        .. exception:: MyException
            :noindex:

            Some doc for *MyException*.

    The exception's payload can be documented using the ``:payload:`` option.
    The ``:label:`` doc field is used like for a type::

        .. exception:: TupleException
            :payload: int * float

            With a tuple payload.

    ..

        .. exception:: TupleException
            :noindex:
            :payload: int * float

            With a tuple payload.

    ::

        .. exception:: RecordException
            :payload: {a: int; b: float}

            With a record payload.

            :label a: a

            :label b: b

    ..

        .. exception:: RecordException
            :noindex:
            :payload: {a: int; b: float}

            With a record payload.

            :label a: a

            :label b: b

    Refer to exceptions using the :rst:role:`exn` role.

Modules and module types
------------------------

.. rst:directive:: .. module:: Name

    Document a module::

        .. module:: MyModule

            Some documentation for *MyModule*.

            .. type:: t

    ..

        .. module:: MyModule
            :noindex:

            Some documentation for *MyModule*.

            .. type:: t
                :noindex:

    The module can be documented as an `alias <https://caml.inria.fr/pub/docs/manual-ocaml-4.05/extn.html#sec235>`_ using the ``:alias_of:`` option.
    There should be no contents in that case::

        .. module:: MyAlias
            :alias_of: Original

            Some documentation for *MyAlias*.

    ..

        .. module:: MyAlias
            :noindex:
            :alias_of: Original

            Some documentation for *MyAlias*.

    If the module get its contents from something else (*e.g* a module type, a functor application, *etc.*),
    this can be documented using the ``:contents_from:`` option::

        .. module:: Contents
            :contents_from: SomeModuleType

            .. type:: t

    ..

        .. module:: Contents
            :noindex:
            :contents_from: SomeModuleType

            .. type:: t
                :noindex:

    Refer to modules using the :rst:role:`mod` role.

.. rst:directive:: .. module_type:: Name

    Document a module type::

        .. module_type:: MyModuleType

            Some documentation for *MyModuleType*.

            .. type:: t

    ..

        .. module_type:: MyModuleType
            :noindex:

            Some documentation for *MyModuleType*.

            .. type:: t
                :noindex:

    The ``:contents_from:`` option is also applicable to module types::

        .. module_type:: Contents
            :contents_from: SomeModuleType

            .. type:: t

    ..

        .. module_type:: Contents
            :noindex:
            :contents_from: SomeModuleType

            .. type:: t
                :noindex:

    Refer to module types using the :rst:role:`modtyp` role.

Functors
--------

.. rst:directive:: .. functor_parameter:: Name

    Document a functor parameter::

        .. module:: Functor

            .. functor_parameter:: Parameter

                .. val:: n
                    :type: int

            .. val:: m
                :type: int

    ..

        .. module:: Functor
            :noindex:

            .. functor_parameter:: Parameter

                .. val:: n
                    :noindex:
                    :type: int

            .. val:: m
                :noindex:
                :type: int

    ::

        .. module_type:: FunctorType

            .. functor_parameter:: Parameter

                .. val:: n
                    :type: int

            .. val:: m
                :type: int

    ..

        .. module_type:: FunctorType
            :noindex:

            .. functor_parameter:: Parameter

                .. val:: n
                    :noindex:
                    :type: int

            .. val:: m
                :noindex:
                :type: int

    The ``:contents_from:`` option is also applicable to functor parameters::

        .. module:: Functor2

            .. functor_parameter:: Parameter
                :contents_from: SomeModuleType

                .. val:: n
                    :type: int

            .. val:: m
                :type: int

    ..

        .. module:: Functor2
            :noindex:

            .. functor_parameter:: Parameter
                :contents_from: SomeModuleType

                .. val:: n
                    :noindex:
                    :type: int

            .. val:: m
                :noindex:
                :type: int

Roles
=====

Some `directives`_ create entries in the :ref:`general index <genindex>` and other `indexes`_.
You can avoid creating entries by using their ``:noindex:`` option. (That's what we've secretly done above, to avoid polluting the indexes.)

.. rst:role:: val

    Refer to a :rst:dir:`val`: ``:val:`Linked.M.v2``` creates this link: :val:`Linked.M.v2`.

    Notice that a dot is used to link to the contents of a :rst:dir:`module`.
    To refer to the contents of a :rst:dir:`module_type`, use a colon: ``:val:`Linked.MT:v3``` produces :val:`Linked.MT:v3`.
    And to refer to the contents of a :rst:dir:`functor_parameter`, use a dollar sign: ``:val:`Linked.M.P$v1``` produces :val:`Linked.M.P$v1`.

    To create shorter references, you can strip the path by prefixing it with a tilde: ``:val:`~Linked.M.v2``` produces :val:`~Linked.M.v2`.
    When there is no ambiguity, you can also omit a prefix of the path: ``:val:`.M.v2``` produces :val:`.M.v2` and ``:val:`.v2``` produces :val:`.v2`.
    The shortened path must start with either a dot, a colon or a dollar sign according to the kind of the previous (omitted) part: ``:val:`$v1``` produces :val:`$v1` and ``:val:`:v3``` produces :val:`:v3`.
    You can combine both: ``:val:`~.M.v2``` produces :val:`~.M.v2`.
    This is consistent with the default `Python Sphinx domain <http://www.sphinx-doc.org/en/stable/domains.html#cross-referencing-python-objects>`_.

.. rst:role:: typ
.. rst:role:: exn
.. rst:role:: mod
.. rst:role:: modtyp

    Referring to other kinds of elements follows the same rules:

    - for :rst:dir:`type`, ``:typ:`.t2``` produces :typ:`.t2`
    - for :rst:dir:`exception`, ``:exn:`.E2``` produces :exn:`.E2`
    - for :rst:dir:`module`, ``:mod:`.M``` produces :mod:`.M`
    - for :rst:dir:`module_type`, ``:modtyp:`.MT``` produces :modtyp:`.MT`

.. module:: Linked

    .. module:: M

        .. functor_parameter:: P

            .. val:: v1

        .. val:: v2
        
        .. type:: t2

        .. exception:: E2

    .. module_type:: MT

        .. val:: v3

Indexes
=======
