# Assumes you're running from the project root.
mkdir -p fuzz/afl_output/fasta

afl-fuzz -i fuzz/test_files/fasta \
	 -o fuzz/afl_output/fasta \
	 -x fuzz/dictionaries/fasta.dict \
	 _build/default/fuzz/fuzz_fasta_in_channel.exe @@
