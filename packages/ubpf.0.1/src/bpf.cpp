#include <cstdlib>
#include <stdexcept>
#include <string>
#include "bpf.h"

extern "C" {
#include "ubpf.h"
}

namespace ubpf
{

static uint64_t
bpf_cast_float(uint64_t f)
{
  union {
    double f;
    uint64_t i;
  } i;
  i.i = f;
  return (uint64_t) i.f;
}

bpf::bpf(void const* p, size_t size, bool is_elf)
{
  vm_ = ubpf_create();
  if (!vm_) throw std::runtime_error("failed to create ubpf vm");

  ubpf_register(vm_, 1, "cast_float", (void *)bpf_cast_float);

  char* errmsg = nullptr;
  int r = is_elf ? ubpf_load_elf(vm_, p, size, &errmsg) : ubpf_load(vm_, p, size, &errmsg);
  if (0 != r)
  {
    std::string err = "failed to load BPF code : ";
    err += errmsg;
    free(errmsg);
    ubpf_destroy(vm_);
    vm_ = nullptr;
    throw std::runtime_error(err);
  }
}

bpf::bpf(bpf&& other)
{
  swap(*this, other);
}

bpf::~bpf()
{
  if (nullptr != vm_) ubpf_destroy(vm_);
}

void bpf::swap(bpf& first, bpf& second)
{
  using std::swap;
  swap(first.vm_, second.vm_);
  swap(first.fn_, second.fn_);
}

void bpf::compile()
{
  if (nullptr != fn_) return;
  char* errmsg = nullptr;
  fn_ = ubpf_compile(vm_, &errmsg);
  if (nullptr == fn_)
  {
    std::string err = "failed to jit BPF code : ";
    err += errmsg;
    free(errmsg);
    throw std::runtime_error(err);
  }
}

uint64_t bpf::exec(void const* mem, size_t mem_size) const
{
  if (NULL == fn_) // not jitted => interpret
  {
    return ubpf_exec(vm_, (void*)mem, mem_size);
  }
  else
  {
    return fn_((void*)mem, mem_size);
  }
}

} // namespace ubpf
