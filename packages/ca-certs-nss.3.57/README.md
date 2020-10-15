# ca-root-nss

Trust anchors extracted from Mozilla's NSS certdata.txt package, to be used
in MirageOS unikernels.

To update trust anchors, please adjust the commit hash to the latest NSS
release in `lib/dune`, remove `certdata.txt`, and run dune build.
