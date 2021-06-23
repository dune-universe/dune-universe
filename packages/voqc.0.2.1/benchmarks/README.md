# VOQC Benchmark Scripts

This directory contains scripts for producing the data in [our paper](https://arxiv.org/abs/1912.02250). Note that the results from these scripts will likely be different from those reported in our paper. For example: timings will be slower due to gate set conversions that were not done at the time of the POPL submission. For the versions of the code used to generate the data in our paper, see the `VOQC/benchmarks` directory in the [POPL2021 branch of inQWIRE/SQIR](https://github.com/inQWIRE/SQIR/tree/POPL2021).

## Setup

1. Follow the instructions in the top-level (`..`) directory to build the voqc_cli executable.
2. Install PyZX, Qiskit, and tket (`pip install pyzx qiskit pytket`)

## Running Scripts

The simplest script is `run_voqc_artifact.sh`. This is also a good script to test if everything is properly installed. If all goes well, then you should see something like the following:
```
##### [Wed Apr  7 19:11:45 EDT 2021] Running VOQC...
	DONE
##### [Wed Apr  7 19:12:23 EDT 2021] Running Qiskit...
	DONE
##### [Wed Apr  7 19:13:54 EDT 2021] Running tket...
	DONE
##### [Wed Apr  7 19:14:09 EDT 2021] Running PyZX...
	DONE
    
<formatted tables>
```
On our test machine (a Macbook Pro running macOS Big Sur with OCaml 4.10.0) this script takes about 15 minutes.

`./run_voqc.sh` will run VOQC, with various options, on *all* the benchmarks in VOQC-benchmarks. Some of the benchmarks are quite large, so this will take a while (~24 hours on our test machine). This script creates files Arithmetic_and_Toffoli_results.csv, PF_results.csv, and QFT_and_Adders.csv to summarize results.

Finally, `validate_outputs.sh` will run VOQC + PyZX translation validation on all the benchmarks in VOQC-benchmarks/Arithmetic_and_Toffoli_partial. (For information on PyZX translation validation see Sec 3.2 of [arxiv:1904.04735](https://arxiv.org/pdf/1904.04735.pdf)). At the time of writing, PyZX's translation validator will not succeed on programs compiled by VOQC from the VOQC-benchmarks/PF directory (see `validate_outputs_fail.sh`). This is most likely the result of different roundoff error in PyZX versus VOQC. VOQC begins by converting float parameters to rational numbers, performs optimization over rationals, and then converts  rationals back to floats with 53 bits of precision (see rzq_to_rz.py).


