## Implementation notes

- Getting a fresh Uint8Array buffer is required in case of the wasm memory
  grows. Otherwise, the old Uint8Array gets invalidated (and we face issues like
  Uint8Array of size 0, and slice does not work)
  (https://users.rust-lang.org/t/wasm-memory-buffer-empty-after-allocating-vec-u8/18112/6).
  It makes sense because Uint8Array is a view over a buffer. If the buffer size changes, the view might be incorrect.
