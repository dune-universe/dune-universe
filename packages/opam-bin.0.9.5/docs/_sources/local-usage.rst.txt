
Local Usage
===========

First Steps
-----------

:code:`opam-bin` will take care automatically of generating binary
packages and using them without supervision. The only thing you need
to do is to configure :code:`opam` to use :code:`opam-bin`. This is
usually done by::

  opam-bin install

This command will do the following modifications:

* Install a copy of :code:`opam-bin` as
  :code:`~/.opam/plugins/opam-bin/opam-bin.exe`, and a link in
  :code:`~/.opam/plugins/bin/opam-bin` (so that :code:`opam bin` also
  works)
* Modify :code:`opam` configuration files to call :code:`opam-bin` everytime
  you install or remove a package. This is done by modifying the hooks
  in :code:`~/.opam/config`. For example::

    post-install-commands:
    [
      "/home/user/.opam/plugins/opam-bin/opam-bin.exe"
      "post-install" name version build-id depends installed-files
    ] {?build-id & error-code = 0}

  Note that the command will remove sandboxing, so you think it shouldn't,
  you will have to modify the files manually.
* Add the local repository of binary packages:

  * :code:`local-bin` for
    :code:`file:///home/user/.opam/plugins/opam-bin/store/repo` to be able to
    install binary packages on user request

After this step, :code:`opam-bin` will create a binary package every
time you compile a new source package, if and only if all the
dependencies have also generated a binary package. It also means that
you won't get binary packages in a switch in which you have already
installed packages without :code:`opam-bin` installed.

Simple Example
--------------

Now that :code:`opam-bin` is installed, let's create a new switch that
will generate binary packages::

  opam switch create test-bin 4.07.1
  opam install alt-ergo -y

We can check that binary packages have been created::

  opam-bin list

gives the following output::

  Binary packages in /home/user/.opam/plugins/opam-bin/store/archives:
  alt-ergo.2.3.2+bin+e36313ee+a8034f9e-bin.tar.gz
  alt-ergo-lib.2.3.2+bin+c8fa4342+e81dc009-bin.tar.gz
  alt-ergo-parsers.2.3.2+bin+716cf20e+b265b234-bin.tar.gz
  base-bigarray.base+bin+4d1fcc87+f2cc4984-bin.tar.gz
  base-threads.base+bin+fab854ad+f2cc4984-bin.tar.gz
  base-unix.base+bin+9223113a+f2cc4984-bin.tar.gz
  camlzip.1.10+bin+f6025bac+28b615e5-bin.tar.gz
  conf-autoconf.0.1+bin+42ea19bc+f2cc4984-bin.tar.gz
  conf-gmp.1+bin+cfddfccd+f2cc4984-bin.tar.gz
  conf-m4.1+bin+67ca415f+f2cc4984-bin.tar.gz
  conf-perl.1+bin+b7a00120+f2cc4984-bin.tar.gz
  conf-pkg-config.1.2+bin+3ae7fa61+f2cc4984-bin.tar.gz
  conf-which.1+bin+8abe2442+f2cc4984-bin.tar.gz
  conf-zlib.1+bin+55cf1879+f2cc4984-bin.tar.gz
  dune.2.6.1+bin+8e4c5a68+17b54c37-bin.tar.gz
  menhir.20181113+bin+1dcc3ac1+cb11610e-bin.tar.gz
  num.1.3+bin+e6840903+a31a8afc-bin.tar.gz
  ocaml.4.07.1+bin+b6e07913+f2cc4984-bin.tar.gz
  ocaml-base-compiler.4.07.1+bin+9dc450ca+237a0dc7-bin.tar.gz
  ocamlbuild.0.12.0+bin+dbf59d87+8ab32167-bin.tar.gz
  ocaml-config.1+bin+560cba57+06f6b799-bin.tar.gz
  ocamlfind.1.8.1+bin+4e9e0eee+d53044bc-bin.tar.gz
  ocplib-simplex.0.4+bin+2e969ade+dad7e6b6-bin.tar.gz
  psmt2-frontend.0.2+bin+e8598f63+5327c084-bin.tar.gz
  seq.base+bin+90db3692+17bae418-bin.tar.gz
  stdlib-shims.0.1.0+bin+b219c1e7+97629a4c-bin.tar.gz
  zarith.1.9.1+bin+e25c81d3+f97da984-bin.tar.gz

Let's retry in another switch, to check if :code:`opam-bin` correctly
install the binary packages::

  $ opam update
  $ opam switch list-available
  ocaml-base-compiler 4.07.1+bin+9dc450ca+237a0dc7
  $ opam switch create test-reloc 4.07.1+bin+9dc450ca+237a0dc7
  $ opam install alt-ergo+bin -y

Another solution is to start from an empty switch::

  $ opam switch create test-reloc --empty
  $ opam install alt-ergo+bin -y

We can also search in the generated binary archives for specific files::

  opam-bin search bin/alt-ergo

and the output is::

  alt-ergo.2.3.2+bin+b7812268+ea4c83e2:file:009664912:reg:bin/alt-ergo

Basic Configuration
-------------------

Let's have a look have the configuration of :code:`opam-bin`::

  opam-bin config

outputs something like::

  Current options (from /home/lefessan/.opam/plugins/opam-bin/config):
    base_url : /change-this-option
    rsync_url : None
    patches_url : git@github.com:OCamlPro/relocation-patches
    enabled : true
    create_enabled : true
    all_switches : true
    version : 1
    switches :
    protected_switches :

We will not discuss the first 2 options that are only useful if you
plan to share the binary packages that you generate. Instead, we can
look at the other options.

:code:`patches_url` is the URL to the :code:`git` repository
containing patches to make packages relocatable. It is used when you
call :code:`opam-bin install` or :code:`opam-bin install patches`. For
example, if you want to use a local copy of this repository, you can
modify it for later use::

  git clone git@github.com:OCamlPro/relocation-patches
  cd relocation-patches
  opam-bin config --patches-url file://$(pwd)
  opam-bin install patches

The next options are used to enable/disable :code:`opam-bin` globally
(:code:`enable`) and to enable/disable creation of binary packages
from source packages (:code:`enable_create`). For example, if you want
to create cached binary archives for the dependencies of a package but
not for the package itself::

  opam-bin config --enable-create
  opam install --deps-only coq
  opam-bin config --disable-create
  opam install coq
  opam-bin config --enable-create

Finally, it is possible to control on which switches :code:`opam-bin`
will be enabled::

  opam-bin config --protected-switches '-,4.07.1-src,*+src'

This will tell :code:`opam-bin` to disable binary packages for switch
:code:`4.07.1-src` and all switches matching the regexp :code:`*+src`.

It is also possible to disable :code:`opam-bin` for all switches
except some of them::

  opam-bin config --not-all-switches --switches '-,*+bin'
