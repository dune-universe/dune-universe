## 1.2.4  (development version)
  - Adapt to Angstrom 0.7.0 API changes
    - This is completely internal
  - Added --only-pick-uid option to rescue mode
    - This allows only picking sbx blocks of specific uid when rescuing
    - This is useful when you only only want to rescue one specific file
  - Renamed --only-pick option to --only-pick-block in rescue mode
  
## 1.2.3  (newest version on OPAM)
  - Message text polishing
  - Fixed division by zero error/exception in progress reporting when number of total units is 0
    - The error/exception affects all four modes when dealing with files of size 0 bytes
  - Added only-pick option to rescue mode
    - This allows asking osbx to only pick some type(s) of blocks during rescuing
  - Added seek position checking code for rescue log file
    - This prevents showing "Unknown error" in rescue mode when logged bytes processed field exceeds the file size
  - File UID hex is printed in upper case
  - Added from, to options to rescue mode
    - This allows specifying range of file to scan for data rescuing
  - Added from, to options to show mode(previously only skip-to option exists for show)
    - This allows specifying range of file to scan for metadata
  - Added force-misalign option to rescue and show mode
    - This allows disabling automatic rounding down of from byte parameter

## 1.2.2  (published on OPAM)
  - Optimized decoding code to speed up reference block scanning
    - Avoids decoding the sbx block in full when already has a block of that type
  - Added no-meta option to decode mode
    - If enabled, saves time by not looking for metadata block actively
  - Added code to handle negative parameters
    - decode
      - show-fail-max
    - show
      - find-max
      - skip-to

## 1.2.1  (published on OPAM)
  - Optmized CRC-CCITT
    - Changed fake\_uint16 to use int instead of int64
    - This makes encoding and decoding significantly faster
    - Other block related operations are also faster in general
  - Added silent option to all modes
    - Only affects progress text printing

## 1.2.0  (superseded by 1.2.1, not published on OPAM)
  - Fixed slightly off progress counting in Decode
  - Optimized decoding code to avoid searching for a reference block twice
  - Added command line option for decode and show
    - Added --show-fail-max for decode
    - Added --find-max      for show
  - Updated show command to also show sbx container version
  - Added position tracking to show mode
  - Added --skip-to for show for skipping to some byte
  - Improved progress reporting
    - Replaced fraction with progress bar
    - Made the rate more readable
    - General overhaul to improve quality
  - Optimized performance of progress printing code and log writing code

## 1.1.1  (published on OPAM)
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
  - Fixed Multihash encoding, decoding of BLAKE2 hashes

## 1.0.1  (published on OPAM)
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

## 1.0.0  (published on OPAM)
  - Base version
