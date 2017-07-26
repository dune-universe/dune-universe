## 1.1.1  (development version, not published yet)
  - Removed dependency on Core\_kernel
    - 1.1.1 uses a custom replacement of Core\_kernel.protect and OCaml Pervasives channels

## 1.1.0  (superseded by 1.1.1, not published on OPAM)
  - Added "show" command
  - Switched from using Core to Core\_kernel
    - Now osbx can be compiled on Windows
  - Switched to a pure OCaml implementation of CRC-CCITT
    - Implementation is translated from libcrc implementation
  - Added support for multiple hash functions
    - added support for sha1 sha256 sha512 and blake2b-512
    - in prior versions, only sha256 was supported
  - Encoding, decoding now try to be "smart" (see their man pages)

## 1.0.1  (version on OPAM)
  - Changed rescue mode to use the original bytes of the file, rather than regenerating the block bytes itself
  - Fixed jbuild file to work on macOS when building
  - Added graceful handling of Ctrl-C breaks in rescue mode
    - log file stays valid after being interrupted
    - in 1.0.0, log file may be empty/broken when osbx is interrupted during rescuing
  - Better progress reporting for encoding, decoding and rescuing
  - Fixed progress reporting issues with rescuing
    - in 1.0.0, rescue mode only does a progress report when it outputs a block, rather than after scanning the block bytes
    - this makes osbx outputs no log when the scanned file section is large and contains no valid blocks
  - Fixed log writing with rescuing
    - in 1.0.0, rescue mode only writes to log file when a valid block is found
    - now it is changed to update log on bytes read, not on blocks written
  - Updated file size retrieval function to act correctly when dealing with block devices
    - Changed File\_utils.getsize to use Core.In\_channel.length instead of Unix.LargeFile.stat
  - Added progress reporting for hash in decode mode

## 1.0.0
  - Base version
