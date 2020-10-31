
Commands
========

You can use :code:`opam-bin` to list all available commands, and
:code:`opam-bin COMMAND --help` for usage of a specific command.

opam-bin clean
--------------

Clear the log, remove all packages and archives from the cache and
store (internal repository), and call :code:`opam update` to
synchronize :code:`opam`::

  $ opam bin clean

You can limit the cleaning by adding these arguments:

* :code:`all`: clean everything as described above
* :code:`log`: clean only the internal log
  (:code:`$OPAMROOT/plugins/opam-bin/opam-bin.log`)
* :code:`store`: clean only the store (packages and archives) and call
  :code:`opam update`

For example::

  $ opam bin clean log


opam-bin config
---------------

Configure options from the command line. :code:`opam-bin` options are stored in
:code:`$OPAMROOT/plugins/opam-bin/config`.

To display most options::

  $ opam bin config
  opam-bin 0.9.2 by OCamlPro SAS <contact@ocamlpro.com>
  Current options (from /home/user/.opam/plugins/opam-bin/config):
    base_url : /change-this-option
    rsync_url : None
    patches_url : file:///home/user/GIT/relocation-patches
    title : Repository of Binary Packages
    enabled : true
    create_enabled : true
    share_enabled: false
    all_switches : true
    version : 1
    switches :
    protected_switches :

To display arguments that can be used to change the options::

  $ opam bin config --help

Here are some common arguments:

* :code:`--enable`/:code:`--disable`: when disabled, :code:`opam-bin`
  will neither patch sources, nor create new binary archives, nor
  re-use cached ones.
* :code:`--enable-create`/:code:`--disable-create`: can be used to
  enable/disable creation of new binary archives. When disabled,
  existing binary archives will still be used when possible
* :code:`--enable-share`/:code:`--disable-share`: can be used to
  enable/disable sharing of file between switches using hard links.
  :code:`opam-bin` uses an heuristic to decide which files should be
  shared. In particular, it will try not to share files that could be
  modified, like configuration files.
* :code:`--base-url URL`: the base url that will be used when creating
  opam files to specify from where the binary archive should be
  downloaded. For example, if you specify :code:`https://x/y`, the
  urls in the opam files will look like
  :code:`https://x/y/archives/package.version.tar.gz`

  Note that, when this argument is used, :code:`opam-bin` will
  automatically modify all the existing opam files in
  :code:`$OPAMROOT/plugins/opam-bin/store/repo` to use the new URLs,
  so this argument can be used after creating the packages to relocate
  them.
* :code:`--patches-url URL`: the URL specifying the location of the
  patches to make packages relocatable. It can be an archive over
  https (:code:`https://`), a Git repository (:code:`git@`) or a local
  directory (:code:`file://`).

  Default is
  :code:`https://www.typerex.org/opam-bin/relocation-patches.tar.gz`.

opam-bin help
-------------

Display help about :code:`opam-bin` and its sub-commands::

  $ opam bin --help

opam-bin info
-------------

Display information on binary packages that have been created by
last :code:`opam` commands::

  $ ./opam-bin info
  Info on binary packages:
  2020/09/14:18:57:25: base-threads.base binary package created
  2020/09/14:18:57:25: base-bigarray.base binary package created
  2020/09/14:18:57:25: base-unix.base binary package created
  2020/09/14:18:58:37: ocaml-base-compiler.4.10.0 binary package created
  2020/09/14:18:58:37: ocaml-config.1 binary package created
  2020/09/14:18:58:37: ocaml.4.10.0 binary package created
  2020/09/14:19:02:03: base-threads.base binary package installed from cache
  2020/09/14:19:02:03: base-bigarray.base binary package installed from cache
  2020/09/14:19:02:03: base-unix.base binary package installed from cache
  2020/09/14:19:02:06: ocaml-base-compiler.4.10.0 binary package installed from cache
  2020/09/14:19:02:06: ocaml-config.1 binary package installed from cache
  2020/09/14:19:02:06: ocaml.4.10.0 binary package installed from cache

The :code:`--tail` argument can be used to monitor the log during calls
to :code:`opam`. The file containing the information is called
:code:`$OPAMROOT/plugins/opam-bin/opam-bin.info`.

opam-bin install
----------------

Install :code:`opam-bin` in opam. It performs the following steps:

* Install the binary of :code:`opam bin` in opam to be used as a
  plugin and for hooks
  (:code:`$OPAMROOT/plugins/opam-bin/opam-bin.exe` with a link from
  :code:`$OPAMROOT/plugins/bin/opam-bin`)
* Install hooks to call :code:`opam-bin` while building, installing
  and removing packages (modify :code:`$OPAMROOT/config`)
* Install the internal repository of :code:`opam-bin` as a repository,
  called :code:`local-bin`, where :code:`opam` can lookup packages.
  The internal repo is :code:`$OPAMROOT/plugins/opam-bin/store/repo`
* Download and extract the set of relocation patches

All these actions can be called indiviually using the following
arguments, respectivelly: :code:`exe`, :code:`hooks`, :code:`repos`,
:code:`patches`.

For example, to install a newly built version of `opam-bin`, you can
just use::

  $ ./opam-bin install exe

If you want :code:`opam-bin` to re-download its set of patches because
a new version is available::

  $ opam bin install patches

opam-bin list
-------------
List binary packages created on this computer::

  $ opam bin list

opam-bin pull
-------------

Pull binary packages from the remote server specified by the
:code:`rsync_url` option (set by :code:`opam bin config --rsync-url
URL`).

This command will copy all files from the URL specified in option
:code:`rsync_url` to the :code:`opam-bin` store
(:code:`$OPAMROOT/plugins/opam-bin/store/`). Since no :code:`--delete`
option is given to :code:`rsync`, the new content will be added over
the existing content. It may be useful to use :code:`opam bin clean
store` before to get rid of existing content if you don't want a
merge.

opam-bin push
-------------

Push binary packages to the remote server, specified by the
:code:`rsync_url` option (set by :code:`opam bin config --rsync-url
URL`)::

  $ opam bin push

It will perform the following actions:

* Generate an HTML index (:code:`index.html`) and an opam index
  (:code:`index.tar.gz`) in every repository present in the store
  (:code:`$OPAMROOT/plugins/opam-bin/store/*` where a sub-dir
  :code:`packages` exists)

* Copy the files from the store to the remote server, adding the
  :code:`--delete` option to :code:`rsync` to get rid of former files if
  necessary.

If the :code:`--merge` argument is provided::

  $ opam bin push --merge

the index files are not generated and the :code:`--delete` option is
not passed to :code:`rsync`, resulting in a merge of the stores. You
may have to call :code:`opam admin index` on the remote server for the
opam index to take into account the new packages.

The :code:`--local-only` argument can be provided to only generate the
index and not copy the files::

  $ opam bin push --local-only

Finally, you can extract a set of packages from the current repository
(:code:`$OPAMROOT/plugins/opam-bin/store/repo`) that are compatible
with a binary package (i.e. not conflicting with it or its
dependencies), using :code:`--extract NAME:PACKAGE.VERSION`::

  $ opam bin push --extract 4.07.1:ocaml-base-compiler.4.07.1

will generate a new repository
:code:`$OPAMROOT/plugins/opam-bin/store/4.07.1` containing only the
packages compatible with the binary packages with prefix
:code:`ocaml-base-compiler.4.07.1` (resulting in this example in a
repository containing only 4.07.1 packages). Index files will be
generated too. A later call to :code:`opam bin push` will push all the
store, including the archives and all the repositories.


opam-bin search
---------------

Search binary packages for information.

For example, to locate which package installs a particular file::

  $ opam bin search :bin/opam-user
  user-setup.0.7+bin+2952509c+02303a0a:file:003390664:reg:bin/opam-user-setup

To locate which package depends on another package::

  $ opam bin search :depend:alt-ergo-lib
  alt-ergo.2.3.2+bin+0de601c3+b837d4ad:depend:alt-ergo-lib:2.3.2+bin+46bc9b0d+e15bd9c3
  alt-ergo-lib+bin.2.3.2:depend:alt-ergo-lib:2.3.2+bin+46bc9b0d+e15bd9c3
  alt-ergo-parsers.2.3.2+bin+e7404faa+00c3ebc6:depend:alt-ergo-lib:2.3.2+bin+46bc9b0d+e15bd9c3

To locate the installed size of an archive::

  $ opam bin search nbytes | grep alt-ergo
  alt-ergo.2.3.2+bin+0de601c3+b837d4ad:total:013055077:nbytes
  alt-ergo-lib.2.3.2+bin+46bc9b0d+e15bd9c3:total:033822968:nbytes
  alt-ergo-parsers.2.3.2+bin+e7404faa+00c3ebc6:total:009382175:nbytes


opam-bin share
--------------

Share files passed as arguments. Sharing is performed by replacing
these files by hard links to identical files in the sharing cache
located at :code:`$OPAMROOT/plugins/opam-bin/share/`.

This command behaves identically to what happens when sharing is
enabled in the configuration. It can be used to share an :code:`opam`
switch that was created before activating the option.

Sharing is only efficient to save disk space when binary packages are
used. Otherwise, building from sources often generates different
binary files, that cannot be shared.

The :code:`--rec` option can be used to pass directories to the
command, that will iterate on their content.

opam-bin uninstall
------------------

Un-install :code:`opam-bin` hooks from :code:`opam` config. Note that
it does not currently remove the :code:`local-bin` directory, so you
may have to do it yourself if you want to::

  $ opam bin uninstall
  $ opam remote remove --all --set-default local-bin


OPAM Hooks
----------

* opam-bin pre-build: Check the sources before building the package to
  decide whether to use an existing binary package

* opam-bin wrap-build:
  Exec or not build commands

* opam-bin pre-install:
  Install cached binary archives if available

* opam-bin wrap-install
  Exec or not install commands

* opam-bin post-install:
  Create a binary archive from installed built artefacts

* opam-bin pre-remove:
  Remove binary install artefacts
