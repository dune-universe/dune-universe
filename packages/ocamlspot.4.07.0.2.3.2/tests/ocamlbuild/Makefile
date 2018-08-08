test: hello.native
	../../ocamlspot hello.ml:l5c8
        ../../ocamlspot dir/dir2/bye.ml:l2c12

hello.native: hello.ml
	ocamlbuild -cflag -bin-annot -I dir/dir2 hello.native

clean:
	ocamlbuild -clean
