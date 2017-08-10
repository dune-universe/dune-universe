# Specification of ocaml-SeqBox

Table of Contents
=================

   * [Specification of ocaml-SeqBox](#specification-of-ocaml-seqbox)
      * [Encoding workflow](#encoding-workflow)
      * [Decoding workflow](#decoding-workflow)
            * [Handling of duplicate metadata/data blocks](#handling-of-duplicate-metadatadata-blocks)
            * [Handling of duplicate metadata in metadata block given the block is valid](#handling-of-duplicate-metadata-in-metadata-block-given-the-block-is-valid)
      * [Rescuing workflow](#rescuing-workflow)
      * [Show workflow](#show-workflow)
      * [To successfully encode a file](#to-successfully-encode-a-file)
      * [To successfully decode a sbx container](#to-successfully-decode-a-sbx-container)
      * [To successfully rescue your sbx container](#to-successfully-rescue-your-sbx-container)

Created by [gh-md-toc](https://github.com/ekalinin/github-markdown-toc)

## Encoding workflow
  1. If metadata is enabled, the following file metadata are gathered from file or retrieved from user input : file name, sbx file name, file size, file last modification time, encoding start time
  2. If metadata is enabled, then a partial metadata block is written into the output file as filler
    - The written metadata block is valid, but does not contain the actual file hash, a filler pattern of 0x00 is used in place of the hash part of the multihash(the header and length indicator of multihash are still valid)
  3. Load version specific data sized chunk one at a time from input file to encode and output(and if metadata is enabled, Multihash hash state/ctx is updated as well(the actual hash state/ctx used depends on hash type, defaults to SHA256)
    - data size = block size - header size (e.g. version 1 has data size of 512 - 16 = 496)
  4. If metadata is enabled, the encoder seeks back to starting position of output file and overwrites the metadata block with one that contains the actual hash

## Decoding workflow
Metadata block is valid if and only if
  - Header can be parsed
  - All metadata fields(duplicate or not) can be parsed successfully
    - Duplicate refers to metadata fields with the same ID
  - All remaining space is filled with 0x1A pattern
  - Version(specifically alignment/block size) matches reference block(see below)
  - CRC-CCITT is correct

Data block is valid if and only if
  - Header can be parsed
  - Version and uid matches reference block(see below)
  - CRC-CCITT is correct

  1. A reference block is retrieved first(which is used for guidance on alignment, version, and uid)
    - the entire sbx container is scanned using alignment of 128 bytes, 128 is used as it is the largest common divisor of 512(block size for version 1), 128(block size for verion 2), and 4096(block size for version 3)
    - if there is any valid metadata block in sbx container, then the first one will be used as reference block
    - else the first valid data block will be used as reference block
  2. Scan for valid blocks from start of sbx container to decode and output using reference block's block size as alignment
    - if a block is invalid, nothing is done
    - if a block is valid, and is a metadata block, nothing is done
    - if a block is valid, and is a data block, then it will be written to the writepos at output file, where writepos = (sequence number - 1) * block size of reference block in bytes
  3. If possible, truncate output file to remove data padding done for the last block during encoding
    - if reference block is a metadata block, and contains file size field, then the output file will be truncated to that file size
    - otherwise nothing is done
  4. If possible, report/record if the hash of decoded file matches the recorded hash during encoding
    - if reference block is a metadata block, and contains the hash field, then the output file will be hashed to check against the recorded hash
      - output file will not be deleted even if hash does not match
    - otherwise nothing is done

#### Handling of duplicate metadata/data blocks
  - First valid metadata block will be used(if exists)
  - For all other data blocks, the last seen valid data block will be used for a given sequence number

#### Handling of duplicate metadata in metadata block given the block is valid
  - For a given ID, only the first occurance of the metadata will be used
    e.g. if there are two FNM metadata fields in the metadata block, only the first (in terms of byte order) will be used

## Rescuing workflow
  1. Scan for valid blocks from start of the provided file using 128 bytes alignment
    - rescue mode rescues all 3 versions of sbx blocks
    - if log file is specified, then
      - the log file will be used to initialize the scan's starting position
        - bytes_processed field will be rounded down to closest multiple of 128 automatically
      - the log file will be updated on every ~1.0 second
    - each block is appended to OUTDIR/uid, where :
      - OUTDIR = output directory specified
      - uid    = uid of the block in hex
    - the original bytes in the file is used, that is, the output block bytes are not generated from scratch by osbx
  2. User is expected to attempt to decode the rescued data in OUTDIR using the osbx decode command

## Show workflow
  1. Scan for metadata blocks from start of provided file using 128 bytes alignment
    - if block scanned has sequence number 0, then
      - if the block is a valid metadatablock, it will be collected
      - up to some specified maximum number of blocks are collected(defaults to 1)
    - else
      - nothing is done
  2. Metadata of collected list of metadata blocks are displayed

## To successfully encode a file
  - File size must be within threshold
    - For version 1, that means  496 * 2^32 - 1 =  ~1.9375 TiB, where 496 is data size, obtained via 512(block size) - 16(header size)
    - For version 2, that means  112 * 2^32 - 1 =  ~0.4375 TiB, where 112 is data size, obtained via 128(block size) - 16(header size)
    - For version 3, that means 4080 * 2^32 - 1 = ~15.9375 TiB, where 4080 is data size, obtained via 4096(block size) - 16(header size)

## To successfully decode a sbx container
  - At least one valid data block for each position must exist
  - If data padding was done for the last block, then at least one valid metadata block must exist and the first block amongst the valid metadata blocks needs to contain a field for the file size in order for truncation of the output file to happen

## To successfully rescue your sbx container
  - Get enough valid sbx blocks of your container such that a successful decoding may take place
