#------------------------------------------------------------------
# wrappeur ocaml pour gbdd
#------------------------------------------------------------------
# n.b. on recompile gbdd.o plutot que d'utiliser la librairie standard
#      du LUSTRE_INSTALL (pour être sur d'utiliser des compilos
#      caml et gcc compatibles)
#------------------------------------------------------------------
# Pour utiliser la librairie, il faut linker avec une commande du style :
#
# pour gcc > 3.* !
#  ocamlopt bdd.cmx $(MES_CMX) gbdd_cml.o gbdd.o -cclib -lstdc++ -o $(MON_PRG)
#
#------------------------------------------------------------------
# SOURCES
#------------
# * bdd.ml => bdd.cmi et bdd.cmx
#             (interface + qq utils en ocaml)
#
# * gbdd_cml.cc => gbdd_cml.o
#             (le gros de l'implementation de bdd.ml
#              en C++ compatible C via macro extern "C")
#
# * gbdd.cc/gbdd.h => gbdd.o
#              (le source du module C++ originel)
#
# * test.ml => un test pour voir ...
#
#------------------------------------------------------------------

#OCAML_INSTALL=$OCAMLLIB

AR=ar

OCAML_LIB=$(OCAMLLIB)
CPP_COMPILER=$(GPP)
CPP_LINKER=$(GPP)


CPP_FLAGS=-g -O0 -fno-operator-names -I$(HOME)/$(HOST_TYPE)/include/ -I$(shell $(OCAMLC) -where)  $(CPP_FLAGS0)  -fPIC

LFLAGS =  -cclib -lstdc++


all : bdd.a libbdd_stubs.a bdd.cmxa bdd.cma

# bdd.cmi : bdd.mli
# 	ocamlopt -c bdd.mli

bdd.cmx : bdd.ml
	$(OCAMLOPT) -c  bdd.ml

test.cmx : test.ml
	$(OCAMLOPT) -c test.ml

gbdd_cml.o : gbdd.h gbdd_cml.cc
	$(CPP_COMPILER) -c $(CPP_FLAGS) gbdd_cml.cc -o gbdd_cml.o

gbdd.o : gbdd.h gbdd.cc
	$(CPP_COMPILER) -c $(CPP_FLAGS) gbdd.cc -o gbdd.o




libbdd_stubs.a : gbdd_cml.o gbdd.o
	$(OCAMLMKLIB)  -o bdd_stubs  $^ 
	ranlib libbdd_stubs.a

#	$(AR) rc libbdd_stubs.a  gbdd_cml.o gbdd.o

%.cmo: %.ml 
	$(OCAMLC) -c $<


bdd.cma: bdd.cmo
	$(OCAMLC) -a -o bdd.cma bdd.cmo

bdd.cmxa : bdd.ml libbdd_stubs.a bdd.cmx
	$(OCAMLOPT) -a  -verbose -cclib -lstdc++  -cclib -lbdd_stubs   \
			 -o bdd.cmxa bdd.cmx



bdd.a: bdd.cmx
	$(AR) rc bdd.a bdd.o
	ranlib bdd.a


test :  bdd.cmx gbdd_cml.o gbdd.o test.cmx 
	$(OCAMLOPT) bdd.cmx test.cmx gbdd_cml.o gbdd.o $(LFLAGS) -o test

clean:
	rm -f *.o *.cmo *.cmi *.cmx *.a *.cma *.cmxa *.so

cp:  
	cp libbdd_stubs.a $(LIBDIRS)/
	cp bdd.a $(LIBDIRS)/
	cp bdd.a $(LIBDIRS)/libbdd.a
	cp bdd.cm* $(LIBDIRS)/
	cp libbdd_stubs.a $(LIB_INSTALL_DIR)/
	cp dllbdd_stubs.so $(LIB_INSTALL_DIR)/
	cp bdd.a $(LIB_INSTALL_DIR)/
	cp bdd.a $(LIB_INSTALL_DIR)/libbdd.a
	cp bdd.cm* $(LIB_INSTALL_DIR)/

install:cp

VERIMAG_INSTALL_DIR=`$(OCAMLC) -where`/lucky

# cp-verimag:  
# 	cp libbdd_stubs.a $(VERIMAG_INSTALL_DIR)
# 	cp bdd.a $(VERIMAG_INSTALL_DIR)
# 	cp bdd.a $(VERIMAG_INSTALL_DIR)/libbdd.a
# 	cp bdd.cm* $(VERIMAG_INSTALL_DIR)




