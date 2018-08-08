==========================================
OCamlSpotter - OCaml source browsing
==========================================

OCamlSpotter is a tool for OCaml source code browsing. 

* You can search the definitions of names of values, functions, data types and modules.
* Emacs and Vim helpers help your browsing via editors.
* Definition search traverses module aliases and functor applications: if module M = N, OCamlSpotter automatically seeks the definition of M.x in N. Very helpful in the modern OCaml programming with lots of modules.

OCamlSpotter 2.x uses \*.cmt and \*.cmti files created by OCaml compiler 4.00.0 or newer with -bin-annot option.

Versions: Use the correct version for your OCaml compiler
================================================================

OCamlSpotter strongly depends on OCaml compiler implementation and its compiler-libs library.
You need use the correct pairs of compiler and OCamlSpotter.

https://bitbucket.org/camlspotter/ocamlspot provides OCamlSpotter branches for each OCaml versions:

* For OCaml 4.01.0, use branch 4.01.0.2.x.y
* For OCaml 4.00.1, use branch 4.00.1.2.x.y
* For OCaml 4.00.0, use branch 4.00.0.2.x.y
* For incoming OCaml 4.02.0, use branch 4.02.0. It is still unstable.
* default : Development version. Sometimes not compilable. Not for you.

Installation
============================

To compile OCamlSpotter::

   % make
   % make opt           (This is optional but recommended)
   % make install     
 
Setup
============================

Emacs users
---------------

Put ``ocamlspot.el`` somewhere, then edit your ``.emacs``::

     ; load-path
     (setq load-path (cons "WHERE-YOU-HAVE-INSTALLED-THIS-ELISP" load-path))
     
     (require 'ocamlspot)
     
     ; tuareg mode hook (use caml-mode-hook instead if you use caml-mode)
     (add-hook 'tuareg-mode-hook
       '(lambda ()
         (local-set-key "\C-c;" 'ocamlspot-query)
         (local-set-key "\C-c:" 'ocamlspot-query-interface)
         (local-set-key "\C-c'" 'ocamlspot-query-uses)
         (local-set-key "\C-c\C-t" 'ocamlspot-type)
         (local-set-key "\C-c\C-i" 'ocamlspot-xtype)
         (local-set-key "\C-c\C-y" 'ocamlspot-type-and-copy)
         (local-set-key "\C-ct" 'caml-types-show-type)
         (local-set-key "\C-cp" 'ocamlspot-pop-jump-stack)))
     
     ; set the path of the ocamlspot binary. If you did make opt, ocamlspot.opt is recommended.
     (setq ocamlspot-command "WHERE-YOU-HAVE-INSTALLED-THE-BINARIES/ocamlspot")
     
     ; Optional: You can also change overlay colors as follows:
     ;  (set-face-background 'ocamlspot-spot-face "#660000")
     ;  (set-face-background 'ocamlspot-tree-face "#006600")


``M-x customize-group`` => ``ocamlspot`` shows majour configurable options.

Vim users
-----------

The author does not use Vim, so there is no official OCamlSpot support for Vim,
but there are several Vim plugins are available:

* https://github.com/simonjbeaumont/vim-ocamlspot
* https://github.com/cohama/the-ocamlspot.vim
* https://github.com/MarcWeber/vim-addon-ocaml/blob/master/autoload/vim_addon_ocaml.vim#L167 This is a part of other OCaml related addons

How to use
===============================

Make ``.cmt*`` files: compile OCaml code with -bin-annot option
-------------------------------------------------------------------------

OCamlSpot uses ``.cmt`` and `.cmti`` files for browsing and they must be created
by OCaml compiler adding ``-bin-annot`` option. There are several ways to make them:

* Add ``-bin-annot`` option to the build script (Makefile, OMakefile, etc)
* or OCaml 4.01.0 or later, use OCAMLPARAM to override OCaml compiler switches:
  in bash, ``export OCAMLPARAM="_,bin-annot=1"``.

Use of ``OCAMLPARAM`` with OCaml compiler newer than 4.01.0 is strongly recommended, 
since it is very an easy way to compile 3rd party softwares with ``.cmt*`` files 
without modifying their build scripts.

Install ``.cmt*`` files along with the other object files
-------------------------------------------------------------------------

As far as you are working only in the directory you develop, having ``.cmt*`` files
there is enough for source browsing.

But once you want to browse other install library source code, you have to install 
the generated ``.cmt*`` files along with the other object files
and ``.mli`` files. You need:

* Fix the build scripts to install ``.cmt*`` files,
* or use SpotInstall tool to copy these files later SpotInstall( https://bitbucket.org/camlspotter/spotinstall ).

Keep ``.cmt*`` and source files
-------------------------------------------------------------------------

Do not remove ``.cmt*`` and source files. They are required for browsing.

For OPAM packages, set ``OPAMKEEPBUILDDIR`` environment variable with non-empty string,
then built files are not removed automatically including ``.cmt*`` files.

Browsing your code
-------------------------------------------------

Compile your OCaml source code with ``-bin-annot`` option, 
then it should create ``*.cmt`` and ``*.cmti`` files.

Emacs users: Open the source code in your Emacs and move the cursor to an identifier
usage, then type ``C-c ;``. If things are properly installed and set up,
Emacs should display the definition of the identifier.

Available Emacs commands:

* ``ocamlspot-query``: Jump to definition   
* ``ocamlspot-type``: Display the type. Same as ``caml-types-show-type`` with ``-annot``
* ``ocamlspot-type-and-copy``: Display the type, then copy it to the kill buffer.
* ``ocamlspot-xtype``: Display the type with id stamps
* ``ocamlspot-use``: Display the identifier's stamps
* ``ocamlspot-pop-jump-stack``: Go back to previous buffer layout. Useful when you are lost during browsing.

Vim users...

If something goes wrong
---------------------------------------------------------------------------

* Use the correct ``ocamlspot`` matching with your OCaml compiler version.
* Compile OCaml modules with ``-bin-annot`` ocaml compiler option.
* Keep the source code and produced cmt/cmti files.
* Install cmt/cmti files along with cmi/cma/cmxa files.
* Use ``ocamlspot.opt`` if you have done ``make opt``. It is much faster than ``ocamlspot``.
* CamlP4 has lots of location issues. In many cases, OCamlSpotter cannot workaround them.
* OCamlSpotter may have its own bugs. You can report problems at https://bitbucket.org/camlspotter/ocamlspot/issues?status=new&status=open .

Note for OPAM users
-----------------------------------------------------

* set OCAMLPARAM to enable ``-bin-annot`` option
* set OPAMKEEPBUILDDIR to keep your source code and ``.cmt*`` files
* use ``spotinstall`` to install ``.cmt*`` files along with other object files.

OCamlSpotter with multiple OCaml versions
---------------------------------------------------

OCamlSpotter is compiler version dependent. So, each version of OCaml compiler,
the corresponding OCamlSpotter is required.

Changing automatically from one to another OCamlSpotter, OPAM users may want to
specify the following shell script as a wrapper. Change the OCamlSpotter location
of your favorite editor config to this.::

    #!/bin/sh
    
    # This is a sample shell script which tries to call the corresponding OCamlSpotter
    # with the current OPAM switch.
    
    DIR=`opam config var bin`
    
    if [ -x $DIR/ocamlspot.opt ]; then 
      $DIR/ocamlspot.opt $*
    else 
      if [ -x $DIR/ocamlspot ]; then 
        $DIR/ocamlspot $*
      else 
        echo "ERROR: No ocamlspot.opt or ocamlspot found at $DIR"
      fi
    fi

Reporting bugs
==============================

OCamlSpotter has bugs. I need your help to fix them.
Please report your issues at 
https://bitbucket.org/camlspotter/ocamlspot/issues?status=new&status=open .

* Please attach the smallest reproducible example as possible.
* Explain which version of OCamlSpot you use. i.e. OPAM version or Repo fingerprint.
* If your code is compiled with CamlP4 and ocamlspot shows you strange locations, probably it is due to CamlP4 location bugs. Check the P4-expanded version whether it is a bug of P4 or OCamlSpotter.
