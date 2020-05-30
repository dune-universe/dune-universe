<!--
pandoc README.md -o README.pdf --variable urlcolor=blue -N --toc
-->
# Plebeia

<!--
pandoc README.md -o README.pdf --variable urlcolor=blue -N --toc
-->

# Overview

*Plebeia* is a functional implementation of a Merkle Patricia tree.  The library allows the manipulation of in-memory functional views of the tree and perform atomic commits of those views to the disk.

## Sub-trees

Plebeia supports sub-trees. That is, a non internal node can be either a leaf with a value or the root of a sub-tree called "Bud".  Trees and sub-trees can be navigated like directories and sub-directories using a *cursor* implemented with a [zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)).

## Storage

The Merkle Patricia trees are persisted to a file on disk by appending nodes.  All nodes take up excatly 256 bits (32 bytes) of disk space making it easy to index them.  Hashes are 224 bit long.

Leaf data are persisted in the same data file as the tree.

<!--
Garbage collection of the tree is implemented with a stop and copy approach. (This GC is not implemented yet.)
-->

## Plebeia and Tezos

Plebeia is not only a generic Merkle Patricia tree implementation but also specialized for version controlled file systems.  It is aimed to be used as a storage subsystem for Tezos blockchain.

# Logical nodes

Here we give logical definitions of Plebeia trees.

Their implementations and limitations are detailed later.

## Segment

A segment is a label of Plebeia tree.  It is a non empty list of `L` (left or 0) and `R` (right or 1).   

The longest possible segment in Plebeia is currently 1815, which is about 227 bytes.  In reality, segments are not so long.  Currently the longest segments possible to store Tezos blockchain state is around 430.

## Bud

A *bud* is a node to store a root or sub tree to give the multi-depth directory structure to Plebeia.  It corresponds with the directory seprator character `/` in the Unix file system.

* The top node of a Plebeia tree is always a bud.
* A bud is either *empty* without any subnode, or has at most one subnode.
* The subnode of a bud, if exists, cannot be a bud nor a leaf.

```
    Empty bud      [/]
```
```
    Non empty Bud  [/]
                    |
                    .   <- the subnode
```

## Leaf

A *leaf* is a node which stores a data and has no subnode.
Plebeia does not assume any structure of the contents:
just a binary blob:

```
    Leaf  <data>
```

## Internal

An *internal* is a node which always has 2 subnodes.
The 2 edges to these subnodes are labeled with `L` (Left) and `R` (Right):

```
    Internal      o
               L / \ R
                .   .   <- the subnodes
```

## Extender

An *extender* is a node which always has 1 subnode.
The edge to the subnode is labeled with a segment.

For the uniqueness of the tree representation, an extender *cannot* have another extender as its subnode:

```
    Extender      o
                 /
                 \ LRL...  <- segment
                 /
                 .    <- the subnode, which is not an Extender
```

## Example

```
              [/]
               |
               o
            L/   \R
            o     o
           /    L/ \R
         RL\   [/]  <3>
           /    |
          <1>   o
              L/ \R
             <2> [/]
```

The above Plebeia tree has the following terminal items:

* Data `1` at `/LRL`.
* Data `2` at `/RL/L`.
* An empty directory at `/RL/R`
* Data `3` at `/RR`.

# Segment encoding

*Segment encoding* `SE(seg)` of a binary representation of a segment `seg` in the following manner:

* Convert its segment directions into a bit stream.  For example, `101110` for `RLRRRL`.
* Prepend `0{i}1` where 0 <= `i` < 8 to make the length of the entire bits a multply of 8.

```
     |<---------- 8n ------------>|
     |000...1|<- segment bits --->|
```

Examples
* `SE(RRRLLL)         = 01111000`
* `SE(RLRLRLRL)       = 0000000110101010`
* `SE(RRRLLLRLRLRLRL) = 0111100010101010`


# Merkle hash format

Each Plebeia node has its *Merkle hash* or just *hash* `h(n)`. 

The size of hash is fixed to 224 bits (28 bytes) for Buds, Leafs, and Internals.

Extenders have hashes with variable length from 240 bits (30bytes) to 2048 bits (256bytes).

The goal is to make the root hash independent of the Patricia implementation. The approach is inspired from the [one described by Vitalik Buterin](https://ethresear.ch/t/optimizing-sparse-merkle-trees/3751/14).

* Let `H(x)== BLAKE2B(x,28)` be a hash function with a 224 bit digest.
* Binary operator `||` is the concatenation of binary chars and strings.
* `take(n,x)` is the prefix of `n` bits of binary string `x`.
* `0` and `1` denote zero and one bit, respectively.
* `b{n}` denotes the `n` sequence of bit `b`.
* `SE(seg)`: segment encoding, a string which encodes `seg`.  Defined above.
* `LEN(s)` : Length of bytes of string `s`, encoded into a byte.

## Leaf

The hash of a leaf is `H(0x00 || v)`.

```
h(Leaf(v)) = H(0x00 || v)
```

or

```
      |<-      H(0x00 || v)        ->|
Leaf  |..............................|
```

* The 8 bits of `0x00` at the head is to avoid [preimage attack](https://en.wikipedia.org/wiki/Preimage_attack).

Example
:    Leaf whose value is `"hello world"`(`0x68656c6c6f20776f`) is `H(0x0068656c6c6f20776f) = 0xf04979d25de53067da4f6096f029c3f42478abff2de8ed5b847a3a02`.


## Bud

The hash of the empty bud is all (224 bits) zeros:

```
h(Bud None) = 0{224}
```

or 

```
          |<-------- 224bits--------->|
Empty bud |000000000..........00000000|
```

The hash of a bud with a subnode of hash `h(n)` is `H(0x02 || h(n))` whose last 2 bits are reset to one:

```
h(Bud (Some n)) = take(222, H(0x02 || h(n))) || 1{2}
```

or

```
     |<-      H(0x02 || h(n))     ->|
Bud  |...........................|11|
```

* The 8 bits of `0x02` at the head is to avoid preimage attacks.
* For the optimization of the storage format, the last 2 bits are reset to 1's.

Example
:    The hash of a Bud with an Internal which has 2 empty Buds is `0x08ca5f45bc5f1720d6aeb69f9a71036757de5dd23ab6a9dde731165f`:
     * The hash of the empty bud is `0x00000000000000000000000000000000000000000000000000000000`.
     * The hash of the internal is `H(0x01 || 0x0000..0 || 0x0000..0 || 0x1c)` with setting the last 2 bits to 0's.  It is `0xdb94a51cfaeeeed79741e32e9bfa7a8debe0a194448c317f2dd6565c`.
     * The hash of the top bud is `H(0x02 || 0xdb94a51cfaeeeed79741e32e9bfa7a8debe0a194448c317f2dd6565c)` with setting the last 2 bits to 1's.  It is `0x08ca5f45bc5f1720d6aeb69f9a71036757de5dd23ab6a9dde731165f`.

## Internal

The hash of an internal node with children hashes `l` and `r` is `H(0x01 || h(l) || h(r) || len(h(r)))` whose last 2 bits are reset to zero:

```
h(Internal(n1,n2)) = take(222, H(0x01 || h(n1) || h(n2) || LEN(h(n2)))) || 0{2}
```

or

```
          |<- H(0x01 || h(l) || h(r) || LEN(h(r))) ->|
Internal  |.......................................|00|
```

* The 8 bits of `0x01` at the head is to avoid preimage attacks.
* `LEN(h(r))` is appended to make the hash safer;  `s1 || s2` is not injective by itself where `s1` and `s2` have variable lengths.
* For the optimization of the storage format, the last 2 bits are reset to 0's.

Example
:    The hash of an Internal which has 2 empty Buds is Example
:    The hash of a Bud with an Internal which has 2 empty Buds is `0xdb94a51cfaeeeed79741e32e9bfa7a8debe0a194448c317f2dd6565c`:
     * The hash of the empty bud is `0x00000000000000000000000000000000000000000000000000000000`.
     * The hash of the internal is `H(0x01 || 0x0000..0 || 0x0000..0 || 0x1c)` with setting the last 2 bits to 0's.  It is `0xdb94a51cfaeeeed79741e32e9bfa7a8debe0a194448c317f2dd6565c`.


## Extender

Extenders have variable length hashes.  The hash of an `Extender(seg, n)` is the hash of subnode `h(n)` followed by the segment encoding `SE(seg)` and the length of the segment encoding `LEN(SE(seg))`:

```
h(Extender(seg,n)) = h(n) || SE(seg)
```

or

```
          |<- hash of the subnode ->|<- segment encoding of seg ->|
Extender  |         h(n)            |00...1|<--- segment bits --->|
```

The hash of an extender cannot exceeds 256 bytes, since it can appear `h(r)` of the hash calculation of Internal nodes.  This leads to the maximum length of the segment encoding: 255 - 28 = 227 bytes.  Since the segment encoding requires at least 1 bit for the `0{0,7}1`, the longest possible segment length in Plebeia is 227 * 8 - 1 = 1815.

Example
:    The hash of an Extender which has an empty Bud with a segment `R` is `0x0000000000000000000000000000000000000000000000000000000003`:
     * The hash of the empty Bud is `0x00000000000000000000000000000000000000000000000000000000`
     * The segment encoding of segment `R` is `00000011` in binary, which is `0x03`


# Node storage

This section explains how the current Plebeia implementation at https://gitlab.com/dailambda/plebeia stores its nodes in a persisted file storage.  Other implementations *need not* to follow the same storage format, but this section should still give some information to explain the characteristics and limits of its logical representation and hash caclulation algorithm.

This implementation can change in future for optimizations and bug fixes, but it *should avoid any change* of the hash calculation of the logical nodes.  If an update changes the hash calculation, then entire Plebeia data files must be converted from the original hash to the new one, which takes very long time.

## Cell

Every node is stored in a *cell*, an array with 256 bit (32 bytes). The constant size makes it easy for nodes to refer to each other using an index in the array.

This leads to a bit of awkwardness (224 bit hashes, the cell size limited to 2^32 - 257, etc.) but the potential benefit is that two nodes can fit in a cache line in 64 bit architectures. 

:::info
If it turns out this doesn't make a noticeable difference, it's straightforward to increase the size of cells to 320 bits, or introduce variable size cells, and remove much of the awkwardness.
:::

## Integer encoding

Integers are embedded in cells using little endian.

## Index

Index is a 32 bit integer to identify the cells of a data file.  It can be from `0` to `2^32 - 257`.  The maximum size of the cell array is `(2^32 - 256) * 32 bytes`, about 274GB.

It is possible to store more cells than this limit by splitting entire data into several data files.

## Index part

The last 32 bits of a node cell are called the *index part* of the node, which often refers to a node linked from the node.  The possible index values are from 0 to 2^32 - 1 - 256.

## Tags

The index part value from 2^32 - 256 to 2^32 - 1 are used for *tags*:

* 2^32 -1 to 2^32 -32 : small leaves, whose contents are stored in the previous cell
* 2^32 -33 to 2^32 -64 : medium leaves, whose contents are stored in the 2 previous cells
* 2^32 -254 : link.  To workaround the restriction of internal node, that is, one of the internal node must be always stored in the previous cell of it.
* 2^32 -255 : large leaves in Plebeia, for values more than 64 bytes.
* 2^32 -256 : empty bud
* Others between 2^32 -256 and 2^32 -1 : reserved

## Layout table

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
internal  |<- first 222 of hash ->|D|0| |<- index of A child --->| (also refers to the previous cell)
extender  |---- seg enc ----->|len|0|1| |<- index of the child ->| (may use some of previous cells)
bud       |<- first 222 of hash ->|1|1| |<- index of the child ->|
empty bud |<- 111111111111111111111 ->| |<- -256 --------------->|
leaf (S)  |<- first 224 of hash ----->| |<- -1 to -32 ---------->| (use the previous cell)
leaf (M)  |<- first 224 of hash ----->| |<- -33 to -64 --------->| (use the two previous cells)
leaf (L)  |<- first 224 of hash ----->| |<- -255 --------------->| (use the previous cell and some others)
link      |<-192 0's->|<-child index->| |<- -254 --------------->|
```

This layout restricts the maximum index number to 2^32-257.  Which is about 4G cells, 128GB file size.

## Internal

* The first 222bits are the prefix of the node's hash.
* appended by 1bit `D`, which denotes which child's index is referred in the index part. 0: left, 1: right.
* appended by 1bit of zero
* followed by 32bits of the index of the one of the children specified by `D`

The other child which is not referred in the index part always stored in the previous cell of the internal node.

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
internal  |<- first 222 of hash ->|D|0| |<- index of A child --->|

(also refers to the previous cell)
```

In almost of all the cases, an Internal is written just after the write of the one of its subndoes.  Some exceptional cases when nodes are shared by hashconsing, the both subnodes cannot be adjacent to the Internal node.  *Link* is introduced to work around them.

## Extender

* The 222nd and 223rd bits are `01`.
* The 6 bits from 216th to 221st bits stores an integer *n* (0 <= *n* < 63) to indicate how many previous cells are used to record the segment of the Extender.
* The segment encoding is stored using the *n* previous cells and the first 216 bits of the Extender cell, which is 0 padded to end at 215th bit of the Extender cell.

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
extender  |---- seg enc ----->|len|0|1| |<- index of the child ->|

(may use some of previous cells)
```

Example:

Suppose we have an Extender whose subnode is stored at index 0x12345678
with a segment `RRRRR...R`, 1000 R's.  Then,

* Segment encoding of the segment is `00000001111...111`, 7 0's followed by 1001 1's.  This has 126 bytes.
* The first 99 bytes (= 126 - 27) of the encoding must be stored in the previous cells of the one for Extender itself.  This requires 4 cells (= (99 + 31) / 32)
* 4 cells have 128 bytes, which is 29 bytes larger than 99 bytes.  The segment encoding must be 29 bytes left padded by 0's.
* The length field of Extender (from 216th to 221st bits) must have `4` which is `000100` in binary:

```
          |<-------   239 0's   -------------->|<---- 17 1's --->|
-4        |0000000000000...00000000000000000000|111111...11111111|
-3        |111111111111111111.........111111111111111111111111111|
-2        |111111111111111111.........111111111111111111111111111|
-1        |111111111111111111.........111111111111111111111111111|

          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
extender  |11111....1111111|000100|0|1| |    0x12345678          |

```

## Leaf

The first 224bits are used to store the hash of the leaf.

### Zero leaf

The index 0 is a special index that is always used to refer to the zero leaf, the leaf with the empty data.  The zero leaf is *not* recorded in the disk.  The actual 0th cell in a data file is used for the header to store some meta data.

### Small leaf

The tag of a small leaf is between 2^32-32 to 2^32-1 inclusive.  The data is stored in the previous cell of the leaf from its head with right padding with 0's.

The size of the data is calculated:

```
    length_in_bytes = 2^32 - tag
```


```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
leaf (S)  |<- first 224 of hash ----->| |<- -1 to -32 ---------->| (use the previous cell)
```

```
Previous cell:
          |< ----   256 bits ----------------------------------->|
------------------------------------------------------------------
data      |<- value contents ------------------------>|000......0|                           |
```

### Medium leaf

The tag of a small leaf is between 2^32-64 to 2^32-33 inclusive.  It denotes the length of the value in bytes, which should be stored in the 2 previous cells of the leaf with right padding with 0's.

The size of the data is calculated:

```
    length_in_bytes = 2^32 - tag
```

The value stored from the head of these 2 previous cells.

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
leaf (M)  |<- first 224 of hash ----->| |<- -33 to -64 --------->|
```

```
Cell at -2 and at -1
          |<-- 256 bits -->|<-- 256 bits -->|
---------------------------------------------
data      |<- value contents ------>|000...0|
```

The medium leaves are introduced to optimize the storage size of public keys of Tezos blockchain whose sizes are 33 and 34 bytes which do not fit with the small leaves.

### Large leaf

The tag is 2^32-255.

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
leaf (L)  |<- first 224 of hash ----->| |<- -255 --------------->|
```

```
Cell at -1
          |<------------------ 256 bits -------------------------->|
--------------------------------------------------------------------
data      |<- the last cell of the last cell chunk for the value ->|
```

The contents of the value is stored in previous cells, forming a linked list of *cell chunks* whose format is defined below.  The sole size limit of values storable in the large leaf is the maximum number of cells allowed in a data file.

### Cell chunk

(I use the word 'chunk' since 'block' means a different thing in the blockchain technology.)

A cell chunk is a contiguous cells.  There is a 6 byte length *footer fields* in the last bytes of each cell chunk:

* The last 4 bytes is the *cdr index* which points to the last cell of the next cell chunk in the chunk list.  If the cdr index is 0, the cell chunk is the last chunk in the list.
* The 2 bytes prior to the cdr index is the *content length* in `uint16`.  It is the number of bytes the cell chunk carries for the value contents.
* The data for the value in a cell chunk is stored from the head of the first cell of the cell chunk.  Number of the cells in the cell chunk is computable from the expression `(content_length + 6 + 31) / 32`.
* The remaining space between the content and the footer is filled with 0's.
* A cell chunk can carry up to 65536 bytes of the value, which consist of 2049 cells.

Cell chunk layout:

```
| cell #1 | cell #2 | .. | the last cell in the chunk (#n)                  |
|         |         | .. |                            | footer fields       |
|         |         |    |                            |<-16bits->|<-32bits->|
------------------------------------------------------------------------------
|<- contents (size bytes) ------------------->|000...0|size      |cdr index |
```

The contents of a value are stored from the last cell chunk in the list whose cdr index is 0.  The head of cell chunk list carries the *last* part of the contents.

Example: TODO

## Bud

The last 32bits is fixed to -256.

### Non-empty bud

* 222 bit prefix of the hash of the node
* followed by 2 bits of ones
* followed by 32 bits of the index of the subnode.

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
bud       |<- first 222 of hash ->|1|1| |<- index of the child ->|
```

### Empty bud

224bits of ones are prepended in front of the 32bits of 2^32 - 256:

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
empty bud |<- 111111111111111111111 ->| |<- -256 --------------->|
```

## Link

Hash-consing may introduce Internal node whose either subnodes cannot be placed prior to the Internal node in the storage.  To work around this, a special Link node with tag -254 is introduced.  One of the sub-nodes is pointed indirectly via this link node which is placed in the previous cell of the internal node:

* Tag is 2^32 - 254
* The index of the subnode is placed from 192nd bit.
* The first 192 bits are 0 filled.

```
          |< ----   224 bits -------->| |<------- 32 bits ------>|
------------------------------------------------------------------
link      |<-192 0's->|<-child index->| |<- -254 --------------->|
```

Exapmle: TODO

## Reserved

The other tags between 2^32-256 and 2^32-1 are reserved for future extension.

```
          |< ----   224 bits ----->| |<--------- 32 bits --------->|
--------------------------------------------------------------------
reserved  |                        | |<- unused btwn -256 and -1 ->|
```

## Header

The header of Plebeia data file occupies the first 3 cells:

* The cell #0 is for identification of the file format.
* The cell #1 and #2 are to record the state of Plebeia tree.  These two cells are the sole cells which are overwritten regularly.

### The cell #0: identifier

```
       |< ----   192 bits --------------->| |<- 32 bits ->| |<- 32 bits ------>|
--------------------------------------------------------------------------------
Cell #0|PLEBEIA\000...................\000| |<- version ->| |<- path version ->|
```

It has 2 versions:

* `version` is the version of the storage format.  The current version is `1`.
* `path version` is used to store the version of the path encoding.

### The cells #1 and #2: header

The cells #1 and #2 store the same contents: the current state of Plebeia tree storage.  These cells are the only cells modified during the operation.  The state is written together with its hash:

```
              |0                   19|20         23|24        27|28        31|
--------------|---------------------------------------------------------------
Cell #1 and #2|<- hash of the right -------------->|<- i-root ->|<- i-next ->|
```

The first 24 bytes of the cell #1 and #2 are the hash (Blake2B24) of the rest of the cell.
The rest of the cell consists of the 2 indices:

* The last index of the root hash record (from bytes 24)
* The index for the next fresh cell (from bytes 28)

The system writes the same contents to these header cells to the Cell #1 and #2 for crash recovery.  If the system restarts from a crash, it checks the header cells and if:

* The both cells are valid (the hashes are correct with respect to the rest of the cells) and equal:  the header information is valid.
* The both cells are valid but not equal: the system has crashed after finishing the write to the Cell #1 and before writing the Cell #2.  We recover using the indices recorded in Cell #1.
* If the Cell #1 is valid but #2 is invalid, then the system has crashed during the write to the Cell #2.  We recover using the indices recorded in Cell #1.
* If the Cell #1 is invalid but #2 is valid, then the system has crashed during the write to the Cell #1.  We recover using the indicves recorded in Cell #2.
* If the both cells are invalid, there is no way to recover.  The system must refuse to restart.

For the performance, the header should not be updated for each DB write.  If the system crashes, then the updates to the DB after the last valid header write are lost, even though they are properly written to the file.

## Root hash records

Plebeia data file keeps the commits together with the tree data.  The information of commits are called "root records".  A root record consists of 2 contiguous cells:

```
Node pointed by i-root or prev:
|0        19|20      23|24        27|28      31|
|<- meta  ->|<- prev ->|<- parent ->|<- idx  ->|

Previous cell:
|0                                           31|
|<------------------- Hash  ------------------>|
```

* Meta is to store information about the commit, such as log message or timestamp.  The format is not specified.
* Prev is the index to the previous root on the file.
* Parent is the index to the top bud of the parent commit, if exists.  If not, it is filled with 0's.  Note that this is NOT the index for the parent's root record.
* Idx is the index to the top bud of this commit.
* Hash is to store the context hash of the commit.   This can be **different** from the Merkle hash of the root node.  Currently in Tezos, this hash is given from the outside i.e. one computed by Irmin2.

The index of the last root hash record is recorded in the i-root field of the header.

:::warning
Do not confuse with `prev` and `parent` fields.  The commit found at `prev` may **not** be the parent of the current commit but a commit of an unrelated branch.  `parent` and `prev`'s `parent` can be different.
:::
