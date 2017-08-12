#!/bin/bash
# corebuild -cflags -safe-string -pkgs stdint,nocrypto,core,hex input_file.byte
pkgs="stdint,nocrypto,nocrypto.unix,core,hex,angstrom,ctypes.foreign"
corebuild -pkgs $pkgs encode.native
corebuild -pkgs $pkgs decode.native
corebuild -pkgs $pkgs single_block_encode.native
corebuild -pkgs $pkgs single_block_decode.native
