
Sharing Packages
================

Private Sharing
---------------

:code:`opam-bin` provides a simple way to share the binary packages
with other users and computers: it automatically creates an
:code:`opam` repository with the binary packages that are created. The
repository is located in :code:`~/.opam/plugins/opam-bin/store/repo`
and the corresponding archives are in
:code:`~/.opam/plugins/opam-bin/store/archives`.

By sharing the directory :code:`~/.opam/plugins/opam-bin/store/` with other
computers, you can simply reuse your binary packages. Here is such an
example session::

  opam-bin clean
  opam switch create 4.07.1
  opam install alt-ergo -y
  rsync -auv --delete ~/.opam/plugins/opam-bin/store other-computer:/tmp/
  rsync -auv --delete ~/.opam/plugins/opam-bin/cache other-computer:/tmp/
  ssh other-computer
  $ opam remote set-url default --all --set-default file:///tmp/store/repo
  $ opam switch create --empty
  $ opam install alt-ergo+bin

Note that we also add to share :code:`~/.opam/plugins/opam-bin/cache` on the
other computer, because without specific configuration, the :code:`url
{ src: }` field in generated :code:`opam` files will not provide the
correct path to the archive. Fortunately, :code:`opam` is able to use
the cache to find the archives.

Public Sharing
--------------

If you want to share your binary packages on a public repository, it
requires almost no additional work: you only need to specify the URL
of your web-server in an option::

  opam-bin config --base-url http://my.server.com/opam-bin

This command will not only change the configuration for newly created
packages, but also modify already generated binary packages to use
this URL for the :code:`url { src: }` field.

After this command, the repository is expected to be copied in
:code:`http://my.server.com/opam-bin/repo`, while archives are
expected to be available in
:code:`http://my.server.com/opam-bin/archives`.

:code:`opam-bin` also provides a simple way to copy the repository on
the remote server. You first need to specify where the files should be
copied, and then use the :code:`opam-bin push` command::

  opam-bin config --rsync-url my.server.com:/var/www/opam-bin
  opam-bin push

By default, this :code:`opam-bin push` will delete all upstream
packages that are not locally present. It will also generate in the
repository a file :code:`index.tar.gz` for :code:`opam`, and a file
:code:`index.html` listing all available packages.

If you don't want this behavior, i.e. you only want to merge new
packages, you should use the :code:`--merge` option, you will also
need to recreate the file :code:`index.tar.gz` upstream::

  opam-bin config --rsync-url my.server.com:/var/www/opam-bin
  opam-bin push --merge
  ssh my.server.com
  $ cd /var/www/opam-bin/repo
  $ opam admin index

Yet, most of the time, you will want to build your packages on top of
former binary packages, so you may want to first download your
upstream repo locally, so that you will be able to avoid the
:code:`--merge` option. For that, you can use :code:`opam-bin
pull`. For example, if you want to add a binary package for
:code:`alt-ergo` and its dependencies::

  opam-bin config --rsync-url my.server.com:/var/www/opam-bin
  opam-bin pull
  opam install alt-ergo -y
  opam-bin push

It is possible to customize the header and trailer of the generated
:code:`index.html` file by defining the files
:code:`~/.opam/plugins/opam-bin/header.html` and
:code:`~/.opam/plugins/opam-bin/trailer.html`.  To test the impact of
changing these files, you can use::

  opam-bin push --local-only

to generate a new
:code:`~/.opam/plugins/opam-bin/store/repo/index.html` file and check
the result.

Using a Binary Repository Only
------------------------------

Once a repository with binary packages has been published,
:code:`opam-bin` is not needed to use it.

For example::

  export OPAMROOT=$HOME/opam-root
  opam init --bare -n https://www.origin-labs.com/opam-bin/debian10.4-amd64/4.10.0
  opam switch create alt-ergo --empty
  opam install alt-ergo -y
