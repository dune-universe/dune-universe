#!/bin/sh -e

# only execute anything if either
# - running under orb with package = builder
# - not running under opam at all
if [ "$ORB_BUILDING_PACKAGE" != "builder" -a "$OPAM_PACKAGE_NAME" != "" ]; then
    exit 0;
fi

basedir=$(realpath "$(dirname "$0")"/../..)
bdir=$basedir/_build/install/default/bin
tmpd=$basedir/_build/stage
rootdir=$tmpd/rootdir
bindir=$rootdir/usr/bin
systemddir=$rootdir/usr/lib/systemd/system
confdir=$rootdir/etc/builder
debiandir=$rootdir/DEBIAN

trap 'rm -rf $tmpd' 0 INT EXIT

mkdir -p "$bindir" "$debiandir" "$systemddir" "$confdir"

# stage app binaries
install $bdir/builder-server $bindir/builder-server
install $bdir/builder-client $bindir/builder-client
install $bdir/builder-worker $bindir/builder-worker
install $bdir/builder-inspect $bindir/builder-inspect

# install service scripts
install $basedir/Linux/builder.service $systemddir/builder.service
install $basedir/Linux/builder-worker.service $systemddir/builder-worker.service

# install templates
install $basedir/packaging/debian/orb-build.template $confdir/orb-build.template

# install debian metadata
install $basedir/packaging/debian/control $debiandir/control
install $basedir/packaging/debian/changelog $debiandir/changelog
install $basedir/packaging/debian/copyright $debiandir/copyright
install $basedir/packaging/debian/conffiles $debiandir/conffiles

dpkg-deb --build $rootdir $basedir/builder.deb
echo 'bin: [ "builder.deb" ]' > $basedir/builder.install
echo 'doc: [ "README.md" ]' >> $basedir/builder.install
