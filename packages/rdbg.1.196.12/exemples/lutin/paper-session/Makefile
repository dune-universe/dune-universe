LIBDIR=-I +rdbg-plugin -I +lutin 
RDBG=../../../rdbgstart.byte  $(LIBDIR) 
SO=-dllib dllpolkag_caml.so -dllib dllpolkal_caml.so \
-dllib dllpolkai_caml.so -dllib  dllcamlidl.so -dllib  dllpolkag.so \
-dllib dllpolkal.so -dllib dllpolkai.so -dllib dllbdd_stubs.so \
-dllib dllEzdl_c_stubs.so \
-dllib libgmp.so -dllib libstdc++.so  



sut.ml:
	lutin testtrace.lut -main sut -ocaml -o sut.ml 

env.ml:
	lutin testtrace.lut -main env -ocaml -o env.ml 

%.cma: %.ml
	 ocamlc  -linkall -a -o $*.cma  $(LIBDIR)   $(SO)  lut4ocaml.cma  $*.ml 

run: sut.cma env.cma 
	export OCAMLRUNPARAM=s=4M,i=32M,o=150; $(RDBG) -init session.ml

clean:
	rm -rf *.ec *.cm* *.log *~ .*~ *.o *rif0 *rif Data  *.res sut.ml env.ml
