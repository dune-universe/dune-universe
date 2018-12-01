OCaml bindings for the [Userspace eBPF VM](https://github.com/iovisor/ubpf).

uBPF is patched to initialize R2 with data size.

```
let exec bpf data =
  let open Ubpf in
  let vm = load (`Bpf bpf) in
  try
    jit_compile vm;
    let ret = exec vm data in
    release vm;
    ret
  with exn ->
    release vm;
    raise exn

(* the following eBPF program returns 1 if the first byte of data is \001 or \002 *)
let raw_bpf = "\
\x15\x02\x06\x00\x00\x00\x00\x00\
\x71\x13\x00\x00\x00\x00\x00\x00\
\x15\x03\x02\x00\x02\x00\x00\x00\
\x15\x03\x01\x00\x01\x00\x00\x00\
\x05\x00\x02\x00\x00\x00\x00\x00\
\xb7\x00\x00\x00\x01\x00\x00\x00\
\x95\x00\x00\x00\x00\x00\x00\x00\
\xb7\x00\x00\x00\x00\x00\x00\x00\
\x95\x00\x00\x00\x00\x00\x00\x00"

exec raw_bpf "\000" = 0

exec raw_bpf "\001" = 1

exec raw_bpf "\002" = 1

exec raw_bpf "\003" = 0
```

See [OCaml eBPF assembler](https://github.com/ygrek/ocaml-bpf) to assemble raw ```Bpf``.
