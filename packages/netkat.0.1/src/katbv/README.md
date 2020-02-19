# KAT + BV ("KAT plus Boolean vectors")
This is an instantiation of `('act, 'test) Ast.exp` where the elements are thought
of as Boolean vectors and `'act` corresponds to modifying some bits of a Boolean
vector and `'test` can correspond to either testing if a Boolean vector lies in
some range (when interpreted as an integer) or testing that some specific bits
of the Boolean vector match specified values.

Note that internally Boolean vectors are represented as sets of integers which
denote the indices on which the Boolean vector is 1. As a consequence, there is
no fixed dimension. Hence a test of the form `x=1` will match all `v` such that
`v_0=1`. 

## Parser syntax
A Boolean vector is a string s<sub>1</sub>s<sub>2</sub>...s<sub>n</sub> where 
s<sub>i</sub> is either 0 or 1. The vector is interpreted as a binary number 
with the rightmost bit as the low bit at the leftmost bit as the top bit. Thus
the Boolean vector 110 is interpreted as 6. In particular the index of s<sub>i</sub>
is n-i. The vector is identified with a string name called a field. We mostly use
`x` and `y` to denote fields.

The parser recognizes the following syntax for KAT+BV Boolean expressions
- `T` `F` -- "True" "False" respectively
- `x=[mask]` where `[mask]` is a string of `{1,0,?}` indicating which bits are 
tested. For example `x=??1?` matches all bit vectors which have a `1` in the 
second bit position.
- `[a]<=x<=[b]` tests that `x` is in the range `[a,b]` where `[a]` and `[b]` are
binary numbers. One can also simply write `x<=[b]` which is parsed as `0<=x<=[b]`.
- `p+q` `p;q` `~p` -- Boolean "disjunction" "conjunction" "negation" respectively

The parser recognizes the following syntax for KAT+BV expressions
- `[Boolean expression]`
- `x:=[mask]` where `[mask]` is a string of `{1,0,?}` indicating which bits to 
update in `x`. For example `x:=0???` updates the fourth bit to be a 0. Note that
higher, unspecified bits are interpreted as `?`. So for example if `y` has 3 bits
then `y:=0` is interpreted as `y:=??0`. To update `y` to the all 0 vector one would
write `y:=000`.
- `p+q` `p;q` `p*` -- union sequence Kleene star respectively
- `if [Boolean expression] then [expression] else [expression]` -- 
`if b then e1 else e2` is parsed as `b;e1+~b;e2`.


## Command line interface
A given KAT+BV expression can be compiled into a corresponding IDD through the 
command line via the command
```
dune exec -- katbv idd --stdin "[KAT+BV expression]"
```
Alternatively one can test equivalence via
```
dune exec -- katbv equiv --stdin "[KAT+BV expression]" "[KAT+BV expression]"
```
For example
```
dune exec -- katbv idd --stdin "0<=x<=100;x:=010+x:=0"
```

## REPL
Start the KAT+BV REPL with
```
dune exec -- katbv repl
```
To see the available commands type `help` in the REPL.

The commands `table_html` and `idd_pdf` will open new programs outside the terminal.
In particular `table_html` opens the default web browser and `idd_pdf` opens the
default pdf viewer.

### Format of tables
A table in the REPL can be rendered with `table_text`. This will produce a table
of the form

```
Table: 
Pattern      |Actions
-------------+-------------------------
xi=0;yj=1;...|[yj←1;xi←1;...];[y0←1;...];
-------------+-------------------------
...
```
where `xi=0` matches any input such that the ith bit (read from the right) of
field `x` is `0`. A given input matches a row in the table if it matches all conditions
(which are separated by semicolons). After matching a row, the input has a set of
actions applied to it. Each square bracket pair `[]` in an entry in the action
column indicates one action: that which corresponds to updating the bits of the
fields as specified. For example `yj←1` is the action resulting from updating 
the jth bit of field `y` to 1. The action `[]` denotes the identity, i.e. no
updates are performed and the input is returned. If an input does not match a row
the input is dropped.


### Format of IDDs
When rendering programs as IDDs using `idd_pdf`, node labels take the form `xi?`
or `xi!`. Here `x` is the field name and `i` is the bit being inspected. The symbol
`?` indicates that the bit is being tested to equal 0, so `xi?` is equivalent to
`xi=1`.


### Evaluation
The command `eval` evaluates the current policy on the specified field values. The
command is followed by specifying values of certain fields. Any unspecified fields
default to the all 0 vector. 