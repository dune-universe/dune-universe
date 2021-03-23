PKG_CONFIG_PATH := $(shell opam config var prefix)/lib/pkgconfig

EXISTS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --exists ocaml-freestanding; echo $$?)

.PHONY: all clean
all: libiopage_freestanding_stubs.a

ifeq ($(EXISTS), 1)
libiopage_freestanding_stubs.a:
	touch $@
else
CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
CFLAGS := $(FREESTANDING_CFLAGS)

OBJS=stub_alloc_pages.o stub_get_addr.o

libiopage_freestanding_stubs.a: $(OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libtcpip_freestanding_stubs.a
