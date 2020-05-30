# Plebeia TODOs

## Hashcons cache size

### Problem

Small sized values (from 1 to 36bytes) on leaves are cached in memory to share the nodes, but the size of the cache table should be now pretty huge without any restriction.  

The cache information is saved to the data file for persistency, but it is not designed for random query.  For caching, they must sit in the memory.

### Current status

A heuristic is installed to wipe out non frequently appearing values from the cache table.  But this is done in a very ad-hoc way.

### Tasks

* Check how much leaves we need if there is no caching.
* Check how much leaves we can reduce if there is a perfect caching, and how much memory is required for the cache table.
* Redesign the heuristics and implement and benchmark them.
* Think about storing part of cache table in other DB, such as Irmin2.

## Segment optimization

### Problem

The segment is implemented as a list of `Left` and `Right`.  Trivial, but inefficient.

### Current status

Segment decoding from bits to the list of `Left` and `Right` is now done lazily, which has significantly improved the traversal speed of nodes to 200%.  The correctness is not very assured, however.  Especially, the equality of segments are no longer possible by simple polymorhpic equality of OCaml.

Yoshihiro is now working on shorter binary representation of segments.  His prelminary result shows a big improvement.

### Tasks

* Check all the comparison of segments and make sure no polymorphic comparison is used.
* Write tests of segments with this lazy segment decoding.
* UNDER DEVELOPMENT: Consider an efficient representation of segments to reduce the cost of deconding and encoding.

## Copy GC

### Problem

There is no GC at this moment.

### Current status

For GC, we first need node traversal.  I built a fast node traversal using the following properties:

Nodes reachable from a commit are either:

* Leaves with small data under 36 bytes (inclusive), managed by the hashcons caching,
* Nodes reachable from the previous commit.  Their indices are smaller than the index of the root node of the previous commit,
* Or, new node created for the commit.  Their indices are strictly larget than the index of the root node of the previous commit.

This propery holds only if each commit is recorded on the disk atomicly, from parents to children.  Otherwise, it takes days.

I wrote a copy tool of Plebeia data file using this node traversal.  The correctness is checkable by comparing top Merkle hashes.  690 million nodes are copied in 2h33m.  The copied data is 22G, 0.1% larger than the original Plebeia data file, which is 1/10.783 of the original context file.

A variant of the copy tool without recalculating Merkle hashes did not demonstrate any significant performance gain.

### Tasks

* Elaborate the above copy algorithm to a copy GC, which only keeps the data of specified set of commits.
* Concurrent GC, or GC with less stop-the-world time.
* Non-copy, in-place GC, or GC requires less extra space.  Should we keep the above property?

# Tezos node integration

Our goal is to integrate Plebeia as a storage system to Tezos node.

## Current status

A node with 2 storage systems, the original and Plebeia, is working properly.  No conflicts are observed between the systems.

* Context hash is calculated by the original system.  Plebeia cannot provide the same, so if Plebeia is going to override it, the protocol has to change the calculation from the specified block level.
* Plebeia performs path compression: it requires the schema of the paths of blockchain state file sytstem.  Currenlty it is given in an ad-hoc way, by a hard coded table.  This does not work if new uses of paths are introduced by updates of the node, especially the protocol amendment.

## Tasks

* The protocol and the storage must talk in a better path format, not in simple strings but in more typeful data.
* Should the protocol reveal all the possible paths to the storage layer?  Is it possible in the current protocol design without much modifications?
* The protocol amendment itself should not migrate the data file from the original to Plebeia at once, since it takes too long time.  It should be better to give the users an option when to migrate.  The migration itself also should not stop the node too long. 

# Tezos protocol amendment for better storage

The current protocol speaks only in simple paths to the storage subsystem, which misses lots of optimization opportunity in the storage.  The storage code is too dependent on the current LMDB+Irmin storage implementation and is hard to change/try other storage subsystems.

Our protocol amendement should be the following general things, not limited to Plebeia:

* Typeful paths talked between the protocol and the storage subsystem.
* The schema of the blockchain state filesystem. (???)
* Clean-up of APIs around Context storage.
* Seemless transition from one to other storage subsytems. (What about contex hashes? They are different in subsystems.)

# Issues must be fixed

* Tezos with plebeia stops sync'ing at the data of 2019-09-1.  If we restarts the node it comes back to sync.  It seems to relate with the testing phase of Babylon, but not sure.  Currently I am re-synching Tezos-plebeia to 2019-08-31 and to make a backup of the data files so that we can replay the problem.
