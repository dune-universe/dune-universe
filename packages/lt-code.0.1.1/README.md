# OCaml-LT-code

OCaml implementation of a Luby Transform code

[API documentation](https://darrenldl.github.io/ocaml-lt-code/)

## Technical details

PRNG and hash used are based on the Lehmer random number generator to achieve good performance.

Degree selection of drops is randomly generated once during construction of encoder,
and randomly regenerated once during resetting. This is to ensure a bad selection
does not have a lasting effect.

The robust soliton distribution is used for the degree selection, with constant
factors tuned to favour low degree count.
There is an additional modification for the case of systematic encoding,
in which degrees of parity drops are multiplied
by a factor proportional to the ratio between data blocks and parity drops
and an additional adjustable factor.
This essentially "amplifies" the coverage of each parity drop due to
the reduced total number of parity drops.

## License

Code files are licensed under the MIT license as specified in the `LICENSE` file

## Acknowledgement/references

- Luby transform code implementation is based on a
  [blog post by François Andrieux](https://franpapers.com/en/algorithmic/2018-introduction-to-fountain-codes-lt-codes-with-python/)
  and the corresponding [repo](https://github.com/Spriteware/lt-codes-python)

- M.Luby, "LT Codes", The 43rd Annual IEEE Symposium on Foundations of Computer Science, 2002. 

- Tirronen, Tuomas (2005). "Optimal Degree Distributions for LT Codes in Small Cases". Helsinki University of Technology.

- Lehmer random number generator implementation is based on sample C99 code described on
  [Wikipedia](https://en.wikipedia.org/wiki/Lehmer_random_number_generator)
