# Ringo

Caches (bounded-size key-value stores) and other bounded-size collections.

## `Cache`

A cache (`Sigs.CACHE`) is a module that implements a key-value imperative store.
A cache is bounded in size – meaning that inserting bindings beyond the cache
capacity removes other bindings – hash-table-like data-structure – meaning that
it holds key-value bindings.

The specific behaviour of the cache is controlled by different options.
Specifically, the replacement policy (which bindings are removed when
supernumerary bindings are inserted), the overflow policy (when are bindings
removed), the accounting policy (how precisely are bindings counted), and the
capacity (how many elements a cache can hold), are all controlled by parameters.


## Cache parameters

- replacement is for defining the replacement policy of a cache and is provided
  when instantiating the `Cache` module.
  
  `LRU` is for "Least Recently Used", meaning that when a supernumerary item is
  inserted in the cache, the least recently used item is removed to make room.
  `FIFO` is for "First-In, First-Out" meaning that when a supernumerary item is
  inserted in the cache, the oldest inserted element is removed to make room.

- overflow is for defining the overflow policy of a cache and is provided when
  instantiating the `Cache` module.
  
  `Strict` means that the cache never holds more element than is specified when
  calling `create`. `Loose` means that the cache may hold more elements than
  specified when calling `create` but that supernumerary elements may be
  collected by the Garbage Collector.

- accounting is for defining the accounting policy of a cache and is provided
  when instantiating the `Cache` module.
  
  `Precise` means that the cache counts its number of elements precisely.
  `Sloppy` means that the cache may count elements that have been `remove`d from
  the cache as still being held by the cache.
  
  Note that when requesting a `Sloppy` cache, the library might give you a
  `Precise` cache if there is no additional runtime cost. In general, `Sloppy`
  caches are more efficient, but (1) they are not adapted to situations where
  you remove many elements and (2) depending on the other parameters they
  might be only as-efficient-as (not more) than `Precise` caches.
  
  Use `Precise` only if you use `remove` a lot or if you need strong
  guarantee on the number of elements.

- capacity is for specifying the size-bound of the cache and is specified when
  calling `Cache.create` to instantiate a cache.


## `Ring`, `Dll`

Both rings and dlls are bounded-size imperative collections. They are similar to
caches but they hold values rather than bindings – meaning there is no `find`
function – and they are dumber – meaning there is no fine control over their
specific behaviour.

They are used internally to implement some components of the caches. They are
exposed because they can be useful.
