# Init OPAM
source .travis-opam-init.sh

# Check OPAM package description
opam lint

# Install
PKG=ocamldap
opam pin add -y --no-action --kind=git $PKG .
opam install -vy $PKG
opam reinstall -vyt $PKG
opam reinstall -vyd $PKG
opam reinstall -vytd $PKG

# Uninstall
opam remove -vy $PKG
