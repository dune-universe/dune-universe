[[!meta title=“Dose Based Applications”]]

[[apt-cudf-get]]
~~~~~~~~~~~~~~~~

.. raw:: html

   <p>

apt-cudf-get is a wrapper that allows one to invoke apt-get with
external solvers while ignoring apt’s pinning.

.. raw:: html

   </p>

[[apt-cudf.conf]]
~~~~~~~~~~~~~~~~~

.. raw:: html

   <p>

The configuration file allows one to define default optimization
criterias for all solvers known by apt-cudf

.. raw:: html

   </p>

[[apt-cudf]]
~~~~~~~~~~~~

.. raw:: html

   <p>

apt-cudf translates back and forth among a CUDF-based dependency solver
and the protocol used by APT to talk with external dependency solvers.
apt-cudf therefore allows one to use any CUDF solver as an external
solver for APT.

.. raw:: html

   </p>

[[buildcheck]]
~~~~~~~~~~~~~~

.. raw:: html

   <p>

dose-builddebcheck determines, for a set of debian source package
control stanzas, called the source repository, whether a build
environment for the packages of the source repository can be installed
on the specified native architecture by using packages from the binary
repository. For this, only package meta-information is taken into
account: build-dependencies and build-conflicts in the source package,
and inter-package relationsships expressed in the binary repository. The
constraint solving algorithm is complete, that is it finds a solution
whenever there exists one, even for multiple disjunctive dependencies
and deep package conflicts. This problem is computationally infeasible
in theory (that is, NP-complete), but can be solved very efficiently for
package repositories that actually occur in practice. Installability of
binary packages is analyzed according to their Depends, Conflicts, and
Provides fields with their meaning as of Debian policy version 3.9.0.
Pre-depends are treated like Depends, and Breaks are treated like
Conflicts.

.. raw:: html

   </p>

[[ceve]]
~~~~~~~~

.. raw:: html

   <p>

Ceve is a generalized metadata parser. It reads package specifications,
extracts package metadata from them, performs some manipulations, and
outputs the package metadata in one of several formats.

.. raw:: html

   </p>

[[challenged]]
~~~~~~~~~~~~~~

.. raw:: html

   <p>

challenged performs a speculative analysis of the repository to identify
those packages that, if upgraded to a specific version, would break a
large number of other packages in the repository. This tool would be
particularly useful during the upgrade of a specific component to
evaluate its impact on the software archive.

.. raw:: html

   </p>

[[coinstall]]
~~~~~~~~~~~~~

.. raw:: html

   <p>

dose-debcoinstall determines whether a set of foreground Debian binary
packages can be installed together given a set of background Debian
binary packages. If yes, then a valid coinstallation set is printed on
standard output. If the –src option is given, then the associated source
packages are printed on standard output instead.

.. raw:: html

   </p>

[[debcoinstall]]
~~~~~~~~~~~~~~~~

.. raw:: html

   <p>

dose-debcoinstall determines whether a set of foreground Debian binary
packages can be installed together given a set of background Debian
binary packages. If a valid coinstallation set exists, than it is
printed on standard output; else the application exists with exit code 1
and prints nothing.

.. raw:: html

   </p>

[[distcheck]]
~~~~~~~~~~~~~

.. raw:: html

   <p>

distcheck determines, for a set of package control stanzas, called the
repository, whether packages of the repository can be installed relative
to the repository according to the inter-package relationsships
expressed in the package control stanzas. The exact set of relevant
control fields and their meaning depends on the type of the repository.
The constraint solving algorithm is complete, that is it finds a
solution whenever there exists one, even for multiple disjunctive
dependencies and deep package conflicts. This problem is computationally
infeasible in theory (that is, NP-complete), but can be solved very
efficiently for package repositories that actually occur in practice.

.. raw:: html

   </p>

[[outdated]]
~~~~~~~~~~~~

.. raw:: html

   <p>

outdated identifies in a debian package repository those packages that
are not installable with respect to that repository by the their
inter-package relationships (dependencies, conflicts, …), and that
furthermore cannot become installable (in the current version) how
matter how the rest of the repository evolves. This means that this
package has to be updated in the repository to ever become installable
again.

.. raw:: html

   </p>

[[smallworld]]
~~~~~~~~~~~~~~

.. raw:: html

   <p>

smallworld computes detailed statistic about the dependency graph. It
accepts

.. raw:: html

   </p>

[[strongdeps]]
~~~~~~~~~~~~~~

.. raw:: html

   <p>

strongdeps computes the strong dependencies and the impact set of a set
of packages. We say that p strongly depends on q if whenever p is
installed then q must also be installed. The impact set of a package q
is the set of all packages p that strongly depend on q.

.. raw:: html

   </p>
