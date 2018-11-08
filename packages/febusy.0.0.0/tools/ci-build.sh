#! /bin/sh

set -e

export command="$1"
export ocaml_version="$2"
if [ "$ocaml_version" != "" ] ; then
    opam switch $ocaml_version
fi
export documentation_output="$3"
if [ "$documentation_output" = "" ] ; then
    export documentation_output="_build/documentation/"
fi
eval `opam config env`

aptget="sudo apt-get"
opam="opam"
if [ "$installs" = "false" ]; then
    aptget="echo not-aptget: "
    opam="echo not-opam: "
else
    export OPAMYES=1
fi

pre_build () {
    $aptget update -qq
    $aptget install -y pandoc wget
    $opam repository add --rank 1 mothership https://github.com/ocaml/opam-repository.git
    $opam config exec -- opam update --yes
    $opam config exec -- opam upgrade --yes
    $opam pin add -n febusy .
    $opam depext -i febusy
    $opam install febusy cmdliner odoc # Also the tests' dependencies
}

test_main=_build/default/src/test/main.exe
build () {
    ocaml please.mlt configure
    jbuilder build @install
    jbuilder build $test_main
    $test_main random-one
}

some_tests () {
    $test_main random-one
    rm /tmp/ro/one.txt
    $test_main random-one --state /tmp/roro.state --show-log
    # rm -fr /tmp/from
    # mkdir -p /tmp/from
    $test_main usage-report --state /tmp/ur.state --from $HOME/ --to /tmp/ur/
    echo file:///tmp/ur/index.html
}

documentation () {
    $test_main build-doc --state _build/webste.state "$documentation_output"
    echo "See file://$PWD/$documentation_output/index.html"
#    mkdir -p public
#    pandoc -i README.md -s -o public/index.html
}

case "$1" in
    "" )
    build ;;
    "all" )
        pre_build
        build
        some_tests
        documentation
        ;;
    * )
        echo "Don't know what to with $1"
        exit 2 ;;
esac

