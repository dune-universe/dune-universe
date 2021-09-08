This directory contains the datalog implementation.

The Datalog grammar that can be uses to test the prover is as follows:
+ identifers (for predicate symbols or variables occuring within an atom) are sequences that start with a letter (upper or lower case) and contains letters (upper or lower case) and the `_` symbol, and possibly ends with a sequence of `'` symbols, or a single symbol among `|` `!` `"` `#` `$` `%` `&` `\'` `*` `+` `-` `/` `<` `>` `?` `@` `\\` `^` ``` ` ```  `~`
+ constants are natural numbers
+ a predicate symbol consists of an idenditierds optionally followed by `/n` where `n` is the arity of the symbol
+ a program consists of a sequence of rules and facts
+ a rule consists of:
  + a head only: an atom followed by a `.`.
	Ex:
	```
	A(1,2,j,k).
	```
  + a head and a body: an atom followed by the `:-` symbol and a comma separated list of atoms ending with a `.`
	Ex:
	```
	S(i,k) :- NP(i,j),VP(j,k).
	```
+ a fact consists of a an atom whose paremeters are constants (i.e., natural numbers) followed by a `.`
  Ex:
  ```
  A(1,2,3).
  ```
	```
