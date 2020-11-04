PKG_CONFIG_PATH := $(shell opam config var prefix)/lib/pkgconfig

EXISTS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --exists ocaml-freestanding; echo $$?)

.PHONY: all clean
all: libfiat_p256_freestanding_stubs.a

ifeq ($(EXISTS), 1)
libfiat_p256_freestanding_stubs.a:
	touch $@
else
CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
DISCOVER_CFLAGS := $(shell sed 's/^(\(.*\))$$/\1/' ../p256/discover/cflags.sexp | tr -d '"')
CFLAGS := -O3 -I../p256 $(DISCOVER_CFLAGS) $(FREESTANDING_CFLAGS)

OBJS=p256_stubs.o

libfiat_p256_freestanding_stubs.a: $(OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libfiat_p256_freestanding_stubs.a
