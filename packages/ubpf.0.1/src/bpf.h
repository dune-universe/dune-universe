#pragma once

#include <stddef.h>
#include <stdint.h>

struct ubpf_vm;
typedef uint64_t (*ubpf_jit_fn)(void *mem, size_t mem_len);

namespace ubpf {

class bpf
{
public:
  bpf(void const*, size_t, bool);
  bpf(bpf&&);
  bpf(const bpf&) = delete;
  bpf& operator=(const bpf&) = delete;
  virtual ~bpf();
  void compile();
  uint64_t exec(void const*, size_t) const;
private:
  struct ubpf_vm* vm_ = nullptr;
  ubpf_jit_fn fn_ = nullptr;
  void swap(bpf&, bpf&);
};

} // namespace ubpf
