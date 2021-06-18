Integration Guide
-----------------

General Principles
~~~~~~~~~~~~~~~~~~

``ps2ocaml`` is the tool you use to convert ProScript into OCaml. It was designed with the following tenets:

* When there is tension between performance, conciseness, idiomatic expression and statement-by-statement equivalence,
  we always choose statement-by-statement equivalence because equivalence makes it relatively easy for any security
  audit team to vet the translated code
* When there is tension between knowingly producing correct code all the time and knowingly producing correct code 99.9%
  of the time, we always choose 100% of the time but we leave an escape hatch that comes with a
  comprehensive **AUDIT NOTICE** easily searchable (``grep -R "AUDIT NOTICE" .``) in the source code.

The tenets reflect the primary focus of both ProScript and ``dirsp`` on secure code.

Statement by statement equivalence means each ProScript statement is translated to one OCaml expression. Here are some examples:

.. list-table:: Statement by statement equivalents
   :widths: 40 40 20
   :header-rows: 1

   * - ProScript Statement
     - OCaml Expression
     - Notes
   * - .. code-block:: javascript

          var a = 1;
          /* ... */
     - .. code-block:: ocaml

          let a = 1 in
          (* ... *)
     -
   * - .. code-block:: javascript

          const Type_key = {
            construct: function(a) {
              return []
            }
          }
     - .. code-block:: ocaml

          module Type_key = struct
            let construct a = []
          end
     -
   * - .. code-block:: javascript

          a.field =
            Type_key.somefunc(a.field);
     - .. code-block:: ocaml

          a.field <-
	    Type_key.somefunc a.field;
     -
   * - .. code-block:: javascript

          a.field === true;
     - .. code-block:: ocaml

          if (a.field = true) then
            ()
          else
            raise (
              Invalid_argument
                "not (a.field = true)"
            )
     - a descriptive assert
   * - .. code-block:: javascript

          a.field === true;
     - .. code-block:: ocaml

          let (_ : bool) =
	    (* no-op
             statement-by-statement
             equivalence *)
	    a.field in () ;
     - if you disable assertions

ps2ocaml Usage
~~~~~~~~~~~~~~

Assuming you ran ``opam install dirsp-ps2ocaml`` and ``eval $(opam env)``, you can run ``dirsp-ps2ocaml``
from your shell.

Otherwise:

* Build the translator with ``dune build src-proscript/dirsp_ps2ocaml.exe``. That
  will produce a standalone executable at ``<dirsp-exchange>/_build/default/src-proscript/dirsp-ps2ocaml.exe``
  which you can run directly or use the shorthand ``dune exec src-proscript/dirsp_ps2ocaml.exe``.
* In everything that follows, use ``dune exec src-proscript/dirsp_ps2ocaml.exe --`` wherever you see
  ``dirsp-ps2ocaml``.

The command line options are:

.. code-block:: text

    dirsp-ps2ocaml <proscript_file>
        [-i <ocaml_interface_module>]
        [-s <ocaml_shims_module>]
        (-p [<module_name>.]<parameter_name>:<parameter_type>)*
        [-disable_strictequals_assertions]
        [-apachev2license <YYYY Owner>]
        (-whitelist_module_for_letin <module_name>)*
        -o <ocaml_output_file>

When you are first starting you should just use the mandatory option (``-o``) and then add additional options
one at a time until your translated OCaml code compiles. Your first command will look something like:

.. code-block:: bash

    dirsp-ps2ocaml -- src-proscript/proscript-messaging/ps/sp.js \
        -o src-proscript/kobeissi_bhargavan_blanchet.ml

For KBB2017 this is done automatically as part of the build, but if we were to invoke it by hand we could run:

.. code-block:: bash

    dirsp-ps2ocaml -- src-proscript/proscript-messaging/ps/sp.js \
        -p "them:t record_them" \
        -p "msg:t record_msg" \
        -p "Type_sendoutput.a:t record_sendoutput" \
        -whitelist_module_for_letin TOPLEVEL \
        -whitelist_module_for_letin HANDLE \
        -disable_strictequals_assertions \
        -o src-proscript/kobeissi_bhargavan_blanchet.ml

Let's go through that complete command invocation from top to bottom:

1. ps2ocaml will take the ProScript file ``src-proscript/proscript-messaging/ps/sp.js`` and translate
   it into an OCaml implementation at
   ``src-proscript/kobeissi_bhargavan_blanchet.ml``.

   The generated implementation file will rely on other files you create manually. Each of those
   manually created files has its own section in the documentation. They are:

   * The `Interface signature`_ file ``kobeissi_bhargavan_blanchet.mli``
   * The `Interface module`_ file ``kobeissi_bhargavan_blanchet_intf.ml`` (the naming is automatically derived from your ``-o`` option
     unless you use the ``-t`` option)
   * The `Shims module`_ file ``kobeissi_bhargavan_blanchet_shims.ml`` (the naming is automatically derived from your ``-o`` option
     unless you use the ``-s`` option)
2. The ``-p`` option lets us add type hints. OCaml is very good at inferring the types without you having to write type hints,
   but occasionally it needs your help. With the options
   ``-p "them:t record_them" -p "msg:t record_msg" -p "Type_sendoutput.a:t record_sendoutput"``:

   * Any ProScript function parameter with the name ``them`` will be annotated with the ``t record_them`` type
     defined in the OCaml interface module described later.
   * Any ``msg`` parameter will be annotated with ``t record_msg``.
   * And any ``a`` parameter in a module ``Type_sendoutput`` will be annotated with ``t record_sendoutput``.
   
   For example, the ProScript function

   .. code-block:: javascript

        tryDecrypt: function(myIdentityKey, myEphemeralKey, them, msg) { /* ... */ }

   becomes

   .. code-block:: ocaml

        let tryDecrypt myIdentityKey myEphemeralKey (them : t record_them) (msg : t record_msg) = begin (* ... *) end

   and

   .. code-block:: javascript

        const Type_sendoutput = {
            /* ... */
            assert: function(a) {
                /* ... */
            }
        };
        const Type_recvoutput = {
            /* ... */
            assert: function(a) {
                /* ... */
            }
        };

   becomes

   .. code-block:: ocaml

        module Type_sendoutput = struct
            (* ... *)
            let xassert (a : t record_sendoutput) = (* ... *)
        end
        module Type_recvoutput = struct
            (* ... *)
            let xassert a = (* ... *)
        end
3. The ``-whitelist_module_for_letin`` option is an escape hatch to add when ps2ocaml cannot safely convert
   the ProScript into equivalent OCaml. ps2ocaml will generate an implementation that won't
   compile, and when you look at the error in the generated source code it will tell you detailed instructions about how
   to fix it, including the specific ``-whitelist_module_for_letin`` option you must use to enable the
   escape hatch.
4. The ``disable_strictequals_assertions`` option is an esoteric option you probably will not have to use.
   It is documented in :ref:`faq_disable_asserts`.


Interface Module
~~~~~~~~~~~~~~~~~~~

For KBB2017 the interface module file was
`src-proscript/kobeissi_bhargavan_blanchet_intf.ml <https://github.com/diskuv/dirsp-exchange/blob/main/src-proscript/kobeissi_bhargavan_blanchet_intf.ml>`_.

The interface module has all the record types for any records your ProScript algorithm implicitly uses.

It also has any undefined parameter types that you use as annotations with the [(-p <parameter_name:parameter_type>)*] option.
- For example, with ``ps2ocaml -p "msg:t record_msg"`` you must have a ``type `a record_msg`` defined.
- For example, with ``ps2ocaml -p msg:string`` nothing is needed since ``string`` is built-in.
- For example, with ``ps2ocaml -p msg:Bigarray.int16_unsigned_elt`` nothing is needed since ``int16_unsigned_elt`` was specified with a named module ``Bigarray``.

An example you would include in the interface module that accounts for implicit record used in the following ProScript KBB2017 algorithm code

.. code-block:: javascript

    construct: function() {
        return {
            valid: false,
            ephemeralKey: Type_key.construct(),
            initEphemeralKey: Type_key.construct(),
            ciphertext: '',
            iv: Type_iv.construct(),
            tag: '',
            preKeyId: 0
        };
    }

is:

.. code-block:: ocaml

    type 'a record_msg =
        { valid : bool
        ; mutable ephemeralKey : 'a
        ; mutable initEphemeralKey : 'a
        ; ciphertext : 'a
        ; mutable iv : 'a
        ; tag : 'a
        ; preKeyId : int
        }

In addition you must define a ``PROTOCOL`` type that declares the signatures for any ProScript functions where OCaml can't infer their types, and
a ``PROTOCOLFUNCTOR`` and ``PROTOCOLMODULE`` that can instantiate it as follows:

.. code-block:: ocaml

    module type PROTOCOL = sig
        (** The type that will be used to represent contiguous bytes in the protocol; typically Bytes.t or Cstruct.t *)
        type t

        (** An internal type representing a decrypted AES message *)
        type t_aes_decrypted

        (* Everything else below is optional, unless OCaml can't compile because it needs a signature for a function *)

        module Type_key : sig
            val construct : 'a -> t
        end
    end

    module type PROTOCOLFUNCTOR = functor (ProScript : Dirsp_proscript.S) ->
        PROTOCOL with type t = ProScript.t

    module type PROTOCOLMODULE = sig
        module Make: PROTOCOL
    end

The ``PROTOCOL`` is also a great place to put in documentation!

Tip: You can instantiate your machine translated module in ``utop`` and then ``#show`` it to generate a complete signature of your ProScript code. Use
that signature as your Protocol, replace any references to ``ProScript.t`` with ``t``, and then document each signature.

Interface Signature
~~~~~~~~~~~~~~~~~~~

For KBB2017 the interface signature file was
`src-proscript/kobeissi_bhargavan_blanchet.mli <https://github.com/diskuv/dirsp-exchange/blob/main/src-proscript/kobeissi_bhargavan_blanchet.mli>`_.

You simply need to ``include`` the ``PROTOCOLMODULE`` of your interface module. Here is an example:

.. code-block:: ocaml

    (* filename: kobeissi_bhargavan_blanchet.mli *)

    (* Use whichever _intf module you defined earlier *)
    (** @inline *)
    include Kobeissi_bhargavan_blanchet_intf.PROTOCOLMODULE

Shims Module
~~~~~~~~~~~~

For KBB2017 the shims module file was
`src-proscript/kobeissi_bhargavan_blanchet_shims.ml <https://github.com/diskuv/dirsp-exchange/blob/main/src-proscript/kobeissi_bhargavan_blanchet_shims.ml>`_.

The shims module needs to contain a ``Make`` functor that defines all functions in your ProScript algorithm
that can't be machine translated into OCaml. Even if all functions can be machine translated, the ``Make`` functor
must still exist.

Here is an example:

.. code-block:: ocaml

    (* filename: kobeissi_bhargavan_blanchet_shims.ml *)

    (* Use whichever _intf module you defined earlier *)
    include kobeissi_bhargavan_blanchet_intf

    module Make (ProScript : Proscript.S) : Protocol = struct
        (**
          In KBB2017, we had the following ProScript algorithm code that did not convert:

          {v
                const UTIL = {
                    newKeyPair: function(id) {
                        const priv = ProScript.Crypto.random32Bytes('aPK' + id);
                        return {
                            priv: priv,
                            pub: ProScript.Crypto.DH25519(priv, [
                                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09
                            ])
                        };
                    }
                }
           v}

           That Javascript function will be replaced by your hand-written [shim_UTIL_newKeyPair]
           in this module.
         *)

        let shim_UTIL_newKeyPair id =
            let priv = ProScript.Crypto.random32Bytes (ProScript.concat [ ProScript.of_string "aID"; id ]) in
            let byte = ProScript.elem_of_char in
            {
                priv = priv;
                pub = ProScript.crypto.xDH25519 priv (ProScript.of_elem_list [
                    byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00';
                    byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00';
                    byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00';
                    byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x00'; byte '\x09'
                ])
            }
