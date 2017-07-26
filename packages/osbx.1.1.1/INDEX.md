**Sbx\_block (sbx\_block.ml, sbx\_block.mli)**
  - Single SBX block construction/access, encoding to bytes and decoding from bytes
  - Submodules
    - Header
    - Metadata
    - Block
    
**Stream\_file (stream\_file.ml, stream\_file.mli)**
  - Provides framework for streamed processing of files
  - Abstracts away low-level file interaction, allows other modules to only construct "processor" to be run by the framework
    - Processor is a function that only deals with input and/or output channel
    
**Encode (encode.ml, encode.mli)**
  - Encode file (and directly output into another file)
  - Relies on processor framework in Stream\_file
  
**Decode (decode.ml, decode.mli)**
  - Decode file (and directly output into another file)
  - Relies on processor framework in Stream\_file
  
**~~Crcccitt\_wrap (crcccitt\_wrap.ml, crccitt.mli)~~**
  - ~~FFI binding to crcccitt.c~~

**Crcccitt (crcccitt.ml, crcccitt.mli)**
  - Pure OCaml implementation of CRC-CCITT
    - Translated from libcrc

**Conv\_utils (conv\_utils.ml, conv\_utils.mli)**
  - Utility functions for converting between different data types/format

**File\_utils (file\_utils.ml, file\_utils.mli)**
  - Utility functions for file access(namely for getting metadata)

**Misc\_utils (misc\_utils.ml, misc\_utils.mli)**
  - Small utility functions used across different modules

**Random\_utils (random\_utils.ml, random\_utils.mli)**
  - Utility functions for generating random bytes etc

**Time\_utils (time\_utils.ml, time\_utils.mli)**
  - Utilitiy functions to get time, etc

**Multihash (multihash.ml, multihash.mli)**
  - Partial multihash implementation (only supporting SHA256 hash)

**Osbx (osbx.ml)**
  - Main program file/entry point

**Osbx\_encode (osbx\_encode.ml)**
  - Functions for encode command

**Osbx\_decode (osbx\_decode.ml)**
  - Functions for decode command

**Osbx\_show (osbx\_show.ml)**
  - Functions for show command

**Param (param.ml, param.mli)**
  - Central place to configure parameters for other modules

**Sbx_specs (sbx_specs.ml, sbx_specs.mli)**
  - Sbx container specification related parameters, functions

