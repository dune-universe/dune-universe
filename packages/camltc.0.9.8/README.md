This library provides ocaml bindings for Tokyo Cabinet.
Notes:

  * it uses a patched 1.4.48 tokyo cabinet from https://github.com/Incubaid/tokyocabinet
    (the git hash is *d303368a54117e07d60836ab3b9822d4def93c22* )

  * only the btree functions were wrapped, because that's what Arakoon needed.

  * License: ALv2 (see LICENSE for details)
