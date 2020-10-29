PKG_CONFIG_PATH := $(shell opam config var prefix)/lib/pkgconfig

CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
CFLAGS := $(FREESTANDING_CFLAGS) \
    -O2 -std=c99 -Wall -Werror

.PHONY: all clean
all: libmirage-solo5_bindings.a

OBJS=alloc_pages_stubs.o clock_stubs.o mm_stubs.o atomic_stubs.o cstruct_stubs.o solo5_block_stubs.o barrier_stubs.o main.o solo5_console_stubs.o checksum_stubs.o solo5_net_stubs.o

libmirage-solo5_bindings.a: $(OBJS)
	$(AR) r $@ $^

clean:
	$(RM) $(OBJS) libmirage-xen_bindings.a
