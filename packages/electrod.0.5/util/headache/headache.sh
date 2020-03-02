#!/bin/bash
# ensures that globbing returns the empty string if there is no match in
# the for loop below
shopt -s nullglob  

for dir in src; do
	cd $dir
	for f in *; do
			# [[]] bash (and zsh ...) extension of PSOIX []
			# =~ : regexp matching
			if [[ $f =~ .*\.(ml$|mll$|mli$|mly$) ]]; then
#				echo $f
				headache -r $f
				headache -c ../util/headache/cfg -h ../util/headache/header $f
		fi
	done
	cd ..
done
