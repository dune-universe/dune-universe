# Blake3

An implementation of the [BLAKE3](https://github.com/BLAKE3-team/BLAKE3/) cryptographic hash function.

## Mac OS

If the compilation fails complaining undefined symbols starting with `_caml`, you should add the following to your `$HOME/.cargo/config`:

```
[target.x86_64-apple-darwin]
rustflags = [
  "-C", "link-arg=-undefined",
  "-C", "link-arg=dynamic_lookup",
]
```
