
Frequently Asked Questions
==========================

This section will grow over time, and should gather replies to
Frequently Asked Questions.

The patch didn't apply correctly. What shall I do ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Most of the time, we didn't have time to test a patch that we made for
a version on other versions of the same package. It's often the case
that they will work without modification.

For a given version of a package, :code:`opam-bin` selects the patch
with the higest version less or equal to the package being installed.
Sometimes, it would be better to use the patch for the version just
higher. This is a good way to solve your problem.

For that, you will have to use a local version of the
:code:`relocation-patches` directory::

   git clone git@github.com:OCamlPro/relocation-patches
   opam bin config --patches-url file://$(pwd)/relocation-patches
   cd relocation-patches/patches/$NAME/
   cp $NEXT_VERSION.patch $VERSION.patch

Why not upstream patches to opam-repository ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Patches in the :code:`relocation-patches` repository should be
upstreamed to :code:`opam-repository` over time, but the problem is
that they should be applied to many versions of the same packages. For
example, the :code:`ocaml-variants` package contains tens of packages
for every version of OCaml. So, it would take a lot of work. Instead,
:code:`opam-bin` is able to automatically patch a range of versions
using a single patch.
