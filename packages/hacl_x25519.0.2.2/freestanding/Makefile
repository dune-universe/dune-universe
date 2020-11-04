PKG_CONFIG_PATH := $(shell opam config var prefix)/lib/pkgconfig

EXISTS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --exists ocaml-freestanding; echo $$?)

.PHONY: all clean
all: libhacl_x25519_freestanding_stubs.a

ifeq ($(EXISTS), 1)
libhacl_x25519_freestanding_stubs.a:
	touch $@
else
CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
CFLAGS := -O3 -I../src/kremlin/include -I../src/kremlin/kremlib/dist/minimal -I../src -DKRML_HOST_TIME $(FREESTANDING_CFLAGS)

OBJS=hacl_x25519_stubs.o Hacl_Hash.o Hacl_Curve25519_51.o Hacl_Ed25519.o

libhacl_x25519_freestanding_stubs.a: $(OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libhacl_x25519_freestanding_stubs.a
