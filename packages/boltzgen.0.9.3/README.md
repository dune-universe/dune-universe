# BoltzGen

*BoltzGen  : Boltzmann sampler test generator* is a tool to generate tests. Given a function signature it generates a random set of call of this function on generated random input. Inputs are generated using a Boltzmann sampler for unlabeled structure.
This project has the following goals:
- Generate values with as little information as possible in most case only its type, the input by the user should be as small as possible;
- The function to test is supposed to be pure; 
- The primary goal was developing automatic grading system for functional programming exercises.

## On Boltzmann sampler
You can read about Boltzmann sampler here https://en.wikipedia.org/wiki/Boltzmann_sampler . The idea is that, given rules that a structure must follow (exactly like a type); a measure of the size of a structure (the number of bits in memory or the length of a string representing it for an OCaml value) and the average size of structures you want to generate a Boltzmann sampler will generate structure such that structures of the same size have the same probability to be generated and the average size is respected.
Don't be afraid of the mathematical abstraction Boltzmann sampler can be implemented in tractable way.   

## Examples
You can try online here: https://www.lacl.fr/~barbot/boltzgen/
Or in a terminal with 
```
>./boltzgen "val sum: int -> int -> int" --value-only -n 10
sum 19 (-7)
sum (-20) (-19)
sum (-8) (-3)
sum (-6) (-11)
sum 2 16
sum 1 (-19)
sum (-2) (-14)
sum (-13) 4
sum (-2) 9
sum (-2) 2
```
In this simple example integer have been generated.
In a more involved example involving recursive algebraic type, we obtain the following:
```
>./boltzgen "type 'a tree = Empty | Node of 'a tree*'a*'a tree 
  val flatten: int list tree -> int tree" --value-only -n 10 -b 10
flatten (Node((Empty,[],Empty)))
flatten Empty
flatten (Node(((Node((Empty,[],Empty))),[0],(Node((Empty,[(-2)],Empty))))))
flatten (Node(((Node((Empty,[12],(Node((Empty,[],Empty)))))),[],Empty)))
flatten (Node((Empty,[],(Node((Empty,[(-12); 1],Empty))))))
flatten (Node(((Node(((Node((Empty,[],Empty))),[],Empty))),[],(Node((Empty,[(-17); (-10)],(Node(((Node(((Node((Empty,[],Empty))),[],(Node(((Node(((Node((Empty,[],(Node((Empty,[(-8); (-4)],Empty)))))),[(-4)],Empty))),[(-11); (-13)],(Node((Empty,[0],Empty))))))))),[],(Node((Empty,[],Empty))))))))))))
flatten (Node(((Node((Empty,[13; (-5); 0],(Node((Empty,[],Empty)))))),[],Empty)))
flatten (Node(((Node((Empty,[(-14)],Empty))),[(-19); (-12); (-10)],Empty)))
flatten (Node((Empty,[],(Node((Empty,[],(Node((Empty,[18],(Node(((Node((Empty,[(-5)],Empty))),[],Empty))))))))))))
flatten (Node((Empty,[],(Node((Empty,[],Empty))))))
```
If given two OCaml implementation files, the tool will check that both of them match the signature given in argument and generate tests for the first function in the signature and report difference of behaviors by the function of the two files.  

## Standard type library 
The following ocaml types are known by boltzgen :
- int, char, float, string, unit
- 'a option
- 'a list
- 'a array
- ('a, 'e) result

additionally new types exists to control sampling with more constraints 
- nat: natural integer
- natp: positive natural integer
- simple_string, id_string, simple_spaced_string: different kinds of string


## To Do, caveat and history
This tool was programmed during the first COVID19 lockdown in France to grade programming exercise in order to make them a bit interactive. The tool features only what I needed for my students and was written in haste thus many caveats remains.
- ***BUG***: When the function to test takes as input a function  `g` the generated function `g` needs to generate at runtime new OCaml value, this requires some `Obj.magic`. It currently works only for base types. Help from someone with knowledge of internal representation of value is welcome.
- Records are not currently supported it should be easy to add
- A way to build new generators for base type easily is lacking.
- No objects, modules, labeled parameters for function
- The documentation is poor
- Ideally, samplers should be derived at compile type 

This project could be used for unit testing it should be integrated in a testing framework it will require some change in the API. 

## Change log

0.9.3. Add recursive mutual type
0.9.2. Polishing and add array
0.9.1. First public release
