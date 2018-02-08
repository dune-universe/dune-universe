OCAMLC_FLAGS = -dtypes -principal -g -ccopt -g
#OCAMLOPT_FLAGS = -dtypes -inline 3
GAMMU_PATH = $(patsubst -I%,-I %,$(shell pkg-config --cflags-only-I gammu))
GAMMU_LIBS = $(addprefix -cclib ,$(shell pkg-config --libs-only-l gammu))

OBJ = .o
PKG_NAME = gammu
STUBS_LIB = gammu_stubs
DOCDIR = ../doc

INSTALL_FILES = META gammu.mli gammu.cmi gammu.cma gammu.cmxa gammu.a \
  gammu.cmx dll$(STUBS_LIB).so lib$(STUBS_LIB).a

.PHONY: all byte native
all: byte native
byte: gammu.cma
native: gammu.cmxa


gammu.cma gammu.cmxa: gammu.cmo gammu.cmx $(STUBS_LIB)$(OBJ)
	ocamlmklib -o $(basename $@) -oc $(STUBS_LIB) $^ -lGammu -lm

$(STUBS_LIB)$(OBJ): $(STUBS_LIB).c
	$(OCAMLC) -c $< $(GAMMU_PATH) $(GAMMU_LIBS)


.PHONY: install uninstall reinstall doc web

install: $(INSTALL_FILES)
	$(OCAMLFIND) install $(PKG_NAME) $(INSTALL_FILES)

uninstall:
	$(OCAMLFIND) remove $(PKG_NAME)

reinstall: $(INSTALL_FILES)
	$(MAKE) uninstall
	$(MAKE) install

doc:
	$(OCAMLDOC) -html -d $(DOCDIR) -colorize-code \
	  $(OCAML_PACKAGES) $(wildcard *.mli)

include ../Makefile.ocaml

clean::
	$(RM) $(wildcard *.so)
	$(RM) $(wildcard $(DOCDIR)/*.html $(DOCDIR)/*.css)
