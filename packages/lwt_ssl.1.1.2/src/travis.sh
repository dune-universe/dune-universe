set -x



# Install system packages.
case $TRAVIS_OS_NAME in
    linux)
        sudo add-apt-repository -y ppa:avsm/ocaml42+opam12
        sudo apt-get update -qq
        sudo apt-get install -qq libev-dev ocaml-nox opam
        ;;
    osx)
        brew update > /dev/null
        brew install libev ocaml opam
        ;;
esac



# Initialize opam.
opam init -ya --compiler=$COMPILER
eval `opam config env`
ocaml -version



# Install dependencies.
opam install conf-libev
opam pin add -y --no-action lwt_ssl .
opam install -y --deps-only lwt_ssl



# Build and install Lwt_ssl. This is the only inherent test.
opam install -y --verbose lwt_ssl
