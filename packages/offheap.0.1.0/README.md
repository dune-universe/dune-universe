# Offheap

`offheap` moves copies of OCaml values off the heap into memory managed by `malloc` and `free`, allowing them to be manually managed and reclaimed. Moving large objects off the heap can potentially improve performance as they are not reachable by the garbage collector, avoiding expensive traversals.

## Interface

* `val copy : 'a -> 'a t`

Creates a copy of an object, returning a handle to it. The handle is required to free the object. The old value is still usable.

* `val get : 'a t -> 'a`

Returns a reference to the copied object from the handle.

* `val free : 'a t -> unit`

Frees the off-heap memory. All references to the copied object are invalid from this point onwards.

## Limitations

The object to be copied cannot reference non-Ocaml values (`Custom_tag` or `Abstract_tag`).

## Acknowledgement

The implementation was inspired by the iterative traversal implemented in the `Obj` module of the OCaml runtime. This library was inspired by the existing `ancient` library, which is unable to handle large objects because of recursive traversals.

## License

The code is published under the MIT License.
