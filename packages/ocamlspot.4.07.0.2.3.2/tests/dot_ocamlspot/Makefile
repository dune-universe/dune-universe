test: build/hello.cmo build/dir/dir2/bye.cmo
	../../ocamlspot hello.ml:l5c8
	../../ocamlspot dir/dir2/bye.ml:l2c12
	../../ocamlspot hello.ml:l6c24

build/dir/dir2/bye.cmo: dir/dir2/bye.ml
	ocamlc -c -bin-annot -I build/dir/dir2 -o build/dir/dir2/bye.cmo $<

build/hello.cmo: hello.ml build/dir/dir2/bye.cmo build/dir/dir2/copied_before_build.cmo
	ocamlc -c -bin-annot -I build/dir/dir2 -o $@ $<

build/dir/dir2/copied_before_build.cmo: build/dir/dir2/copied_before_build.ml
	ocamlc -c -bin-annot $<

build/dir/dir2/copied_before_build.ml: dir/dir2/copied_before_build.ml
	cp $< $@

clean:
	find build -iname '*.cm*' | xargs rm

