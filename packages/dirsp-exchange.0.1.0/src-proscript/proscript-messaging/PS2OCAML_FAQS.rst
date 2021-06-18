FAQs
----

Question 1. How do I know I got the record types and the mutability right?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Compile the code with ``dune build``. You can rely on OCaml's compile-time type safety.

If it doesn't compile, you are missing a record, you are missing a field, you have the wrong field type, or you didn't make the field
mutable.

Question 2. What is ``ProScript.t``?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It represents what in ProScript would be the memory structure of a string.
The Ocaml type is hidden in ProScript.t, but may be an OCaml ``char array`` or it may be the more
efficient `bytes <https://ocaml.org/api/Bytes.html>`_ depending on the version of the ProScript
runtime.

*All* strings in your ProScript code replaced with ``ProScript.t`` in generated OCaml code. And *all*
lists of element accesses like:

.. code-block:: javascript

    return [ a[0], a[1], a[2] ];

will be converted to use ``ProScript.t``.

.. _faq_integer_safety:

Question 3. What about the runtime safety of 'int' operations?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OCaml ints are machine and compiler specific sizes (ex. 32-bit) and are not normally checked for overflow.
However, the ps2ocaml code generator will generate overflow safety checks if you write a standalone ProScript
statement like:

.. code-block:: ocaml

    a.preKeyId + 1;

The generated code will insert an assertion that makes sure that the "+" operation will not overflow or underflow.
Look in the "assert" functions of ``ps/sp.js`` for ProScript examples, and then look in
``ps/kobeissi_bhargavan_blanchet.ml`` for the corresponding auto-generated overflow checks.

.. _faq_disable_asserts:

Question 4. How do I disable assertions?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ps2ocaml will generate ``Invalid_argument`` exceptions rather than asserts.

When you write a standalone ProScript equality statement like:

.. code-block:: ocaml

    a.valid === true;

the translation is:

.. code-block:: ocaml

    if (a.valid = true) then ()
    else raise (Invalid_argument "not (a.valid = true)")

When you don't want to raise the exception, you can use the ``-disable_strictequals_assertions``
option in ``dirsp-ps2ocaml.exe``. That will produce the following translation:

.. code-block:: ocaml

    let (_: bool) = (* no-op statement-by-statement equivalence *) a.valid in (* ... *)

NOTE: With the current version of ps2ocaml you *cannot disable* Invalid_argument when an :ref:`overflow
or underflow is detected <faq_integer_safety>`.

Question 5. How do I annotate function parameters with explicit types?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You may be forced to annotate if your ProScript code is
`sharing field names in multiple record types <https://dev.realworldocaml.org/records.html#reusing-field-names>`_.

When you run ``ps2ocaml``, use the ``-p`` option to map function parameter names to field types.

For example, if you have the ProScript code:

.. code-block:: javascript

    deriveSendKeys: function(them, myEphemeralKeyPriv) {
        return {
            sendKeys: sendKeys,
            kENC: myEphemeralKeyPriv[0]
        };
    }

then using

.. code-block:: bash

    dirsp-ps2ocaml.exe -p them:record_them ...

will produce the OCaml code:

.. code-block:: ocaml

    let deriveSendKeys (them : record_them) myEphemeralKeyPriv = begin
        {
            sendKeys = sendKeys;
            kENC = myEphemeralKeyPriv.(0)
        }
    end

The better option is to rename the field names so that the field names are unique across all records.
