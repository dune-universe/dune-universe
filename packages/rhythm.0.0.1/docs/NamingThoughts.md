I'm trying to piece together naming schemes and fixing a few problems that I've been running into. I'll be explicit about the naming conventions I'm trying to set and the problems so that you can provide feedback on them.

# Array access syntax

It was annoying to have the `arr[index]` syntax *always* be safe. In an attempt to solve this there is initially no syntax support exposed, then there are multiple kinds of syntax support that are exposed that can be opted into by opening those modules. This has two big upsides:

- People can opt into a convenient unsafe syntax when needed
- We can overload this syntax for any indexed structure without having to worry about conflicts

Right now there are 4 modules that I think need to be exposed for every indexed structure.

- `Struct.Syntax` is the standard safe syntax that returns an option on invalid access. This has two problems solved with each of the following syntaxes:
  - Optional types are tedious to use when bounds checking is done outside of accesses. Usually in optimized/imperative code.
  - This syntax does not work with 2D+ structures, e.g: `array(array('a))`.
- `Struct.SyntaxExn` is the standard unsafe syntax (standard in OCaml) and will throw on invalid indices. It follows the same `Exn` naming convention as functions that might throw.
- `Struct.SyntaxNested` accepts an optional struct as input. This can be annoying for the first access, but because the access is safe and returns an optional type it allows easy and **safe** accessing into nested structures.
- `Struct.SyntaxReset` resets the syntax so it can not be used in the remainder of the scope.

# Switching on non-standard structures

One of the big benefits of the native list is the exhaustive switches over the structure, and other structures that lack this ability suffer. If we want to add more performant structures it would be nice to have the ability to switch over them like we can with lists.

I've added `Struct.matchN` and `Struct.matchNExn` functions (where N is a fixed integer 1-7 for now). This will make an N + 1 tuple where the first N positions are occupied by the first N elements of the struct. The last position in the tuple is occupied by a modified structure with the first N elements removed (i.e. the rest of the elements). This is all wrapped in an optional type, if there are less than N elements then `None` is returned, otherwise `Some(tuple)` is returned (the `Exn` versions are not wrapped in an optional type). This allows matching like:

```
switch (MutableArray.match2(arr)) {
| Some((0, 1, rest)) => true
| Some((1, _, rest)) => true
| _ => false
};
```

This should be easy to extend to any structure with an ordering.

# Naming conventions

## Argument ordering

- Functions that operate on one instance of a structure accept it as the last argument (t-last and `|>`).
- Functions that operate on multiple instances of a structure accept them in left to right order.
  - e.g: `concat([a], [b])` is `[a, b]` not `[b, a]`.

## General conventions

- Modules for data structures that are mutable in any way should be prefixed with `Mutable`, all others are assumed to be entirely immutable.
  - This applies to default structures. Standard `Array` should be hidden and renamed `MutableArray`.
- Functions that might throw an exception should be suffixed with `Exn`, all other functions should be safe.
- Functions that accept a lambda that uses the index should be suffixed with a little `"i"`.
  - e.g: `map(el => el, struct)` and `mapi((i, el) => el, struct)`.
  - In these lambdas the index should come directly before the element.
- When referring to parts of an ordered structure use `first` and `last`.
- When checking the cardinality of a well ordered, linear structure use `length`, otherwise use `size`.
  - A tree can be ordered, but not linear, so would use `size`.

## Creation functions

- `make()` is always a function that takes a unit (or optional named args, then unit) and returns an empty structure.
  - `empty` as a static value is never exposed, always use `make()`.
  - Some structures had problems with `empty` and type generality, we can just always use `make()` to avoid this, and then for consistency we never expose an `empty` value. (I had a specific example, I will see if I can find and reference it).
- `init(n, f)` creates a structures of length `n`, with each index `i` initialized by calling `f(i)`
  - This only applies to indexed structures.

## Conversion functions

> TODO: Conventions for things like `toArray`, `fromList`, and other structures still need to be determined. For example should each structure only have "to" functions? Or should they have both "to" and "from" functions? Is that possible to achieve in some way without having cyclic dependencies?

## Access functions

- Functions that access elements should start with `get`.

## Update functions

- Functions that add elements should start with `add`.
- Functions that change an existing element should start with `set` or `update`.
  - `set` is for directly changing the element to a value.
  - `update` is for calling a function with the element and updating to the result of that function.
- Functions that remove elements should start with `remove`.

## Iteration functions (no changes)

- Standard iteration should be called `forEach` and return a unit.
- Testing predicates should be named `every`, `some`, and `none`.

## Filter functions

The name `filter` can be confusing and hard to remember how exactly it behaves. Filter is a well recognized function name though and should be the prefix of functions that accept a predicate that returns a boolean to determine whether elements should be kept or not. My proposal to improve these names would be:

- When the predicate returning true means to keep the element call the function `filterKeep`
  - This is the standard `filter` functions.
  - `true` means to "keep" the element.
- When the predicate returning true means to remove the element call the function `filterDrop`
  - `true` means to "drop" the element.

## Sequence functions (changes on 1 struct)

- Standard functions `map` and `reverse`.
- Use `reduce` and `reduceReverse` instead of the term `fold`.
  - This is a somewhat arbitrary choice to bring familiarity to JS developers.

## Multi-sequence functions (changes on more than 1 struct)

- `concat` operates on exactly two structs, not a list or N many structs.
- `flatten` operates on a 2D structure and concatenates them all together.
  - `concatMany` should not exist, it is mostly superflous with `flatten`.
    - The main argument would be to always accept a native list of structs to `concatMany` because lists have syntax support. `flatten` would always use a 2D struct of the kind you are using which may be less performant for complex structures.

## Other functions to consider (todo)

> TODO: Conventions on `zip`, `unzip`, `zip2`, `unzip2`, etc.

> TODO: Conventions on `map2`, `map3`, `map2i`, etc.

> TODO: Conventions on `group`, `groupBy`, other "by" functions.

> TODO: Other function ideas to explore: Split, split on, join, chunk, group, find/search, has/includes, flatMap, interleave, splice, index of, last index of, min, max, filterNone, mapSome, slice, insertIndex, count
