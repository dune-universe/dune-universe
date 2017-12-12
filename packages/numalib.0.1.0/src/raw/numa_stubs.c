#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/threads.h>
#include <caml/custom.h>
#include <numa.h>
#include <stdio.h>
#include <pthread.h>

/* thread safety */

static pthread_mutex_t lib_lock = PTHREAD_MUTEX_INITIALIZER;

static void lib_mutex_lock(void)
{
  pthread_mutex_lock(&lib_lock);
}

static void lib_mutex_unlock(void)
{
  pthread_mutex_unlock(&lib_lock);
}


/* int numa_bitmask_isbitset(const struct bitmask * mask, unsigned int bit)
 */
CAMLprim value caml_numa_bitmask_isbitset(value ml_mask, value ml_bit)
{
  CAMLparam2(ml_mask, ml_bit);
  int ret;
  struct bitmask * mask;
  unsigned int bit;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));
  bit = (unsigned int)Int_val(ml_bit);

  caml_release_runtime_system();
  ret = numa_bitmask_isbitset(mask, bit);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* struct bitmask * numa_bitmask_setall(struct bitmask * mask)
 */
CAMLprim value caml_numa_bitmask_setall(value ml_mask)
{
  CAMLparam1(ml_mask);
  struct bitmask * mask;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  (void)numa_bitmask_setall(mask);
  caml_acquire_runtime_system();

  CAMLreturn(ml_mask);
}

/* struct bitmask * numa_bitmask_clearall(struct bitmask * mask)
 */
CAMLprim value caml_numa_bitmask_clearall(value ml_mask)
{
  CAMLparam1(ml_mask);
  struct bitmask * mask;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  (void)numa_bitmask_clearall(mask);
  caml_acquire_runtime_system();

  CAMLreturn(ml_mask);
}

/* struct bitmask * numa_bitmask_setbit(struct bitmask * mask, unsigned int bit)
 */
CAMLprim value caml_numa_bitmask_setbit(value ml_mask, value ml_bit)
{
  CAMLparam2(ml_mask, ml_bit);
  struct bitmask * mask;
  unsigned int bit;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));
  bit = (unsigned int)Int_val(ml_bit);

  caml_release_runtime_system();
  (void)numa_bitmask_setbit(mask, bit);
  caml_acquire_runtime_system();

  CAMLreturn(ml_mask);
}

/* struct bitmask * numa_bitmask_clearbit(struct bitmask * mask, unsigned int bit)
 */
CAMLprim value caml_numa_bitmask_clearbit(value ml_mask, value ml_bit)
{
  CAMLparam2(ml_mask, ml_bit);
  struct bitmask * mask;
  unsigned int bit;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));
  bit = (unsigned int)Int_val(ml_bit);

  caml_release_runtime_system();
  (void)numa_bitmask_clearbit(mask, bit);
  caml_acquire_runtime_system();

  CAMLreturn(ml_mask);
}

/* unsigned int numa_bitmask_nbytes(struct bitmask * mask)
 */
CAMLprim value caml_numa_bitmask_nbytes(value ml_mask)
{
  CAMLparam1(ml_mask);
  unsigned int ret;
  struct bitmask * mask;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  ret = numa_bitmask_nbytes(mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int((int)ret));
}

/* unsigned int numa_bitmask_weight(const struct bitmask * mask)
 */
CAMLprim value caml_numa_bitmask_weight(value ml_mask)
{
  CAMLparam1(ml_mask);
  unsigned int ret;
  struct bitmask * mask;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  ret = numa_bitmask_weight(mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int((int)ret));
}

static void bitmask_finalize(value ml_value)
{
  struct bitmask **pmask;

  pmask = (struct bitmask **)Data_custom_val(ml_value);
  if (*pmask != NULL)
  {
    numa_bitmask_free(*pmask);
    *pmask = NULL;
  }
}

static struct custom_operations bitmask_custom_ops = {
  "bitmask",
  bitmask_finalize,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value pmask_to_ml(struct bitmask *mask)
{
  CAMLparam0();
  CAMLlocal1(ml_mask);
  struct bitmask **pmask;

  ml_mask = caml_alloc_custom(&bitmask_custom_ops, sizeof mask, 0, 1);
  pmask = (struct bitmask **)Data_custom_val(ml_mask);
  *pmask = mask;

  CAMLreturn(ml_mask);
}

/* struct bitmask * numa_bitmask_alloc(unsigned int n)
 */
CAMLprim value caml_numa_bitmask_alloc(value ml_n)
{
  CAMLparam1(ml_n);
  struct bitmask *mask;
  unsigned int n;

  n = (unsigned int)Int_val(ml_n);

  caml_release_runtime_system();
  mask = numa_bitmask_alloc(n);
  caml_acquire_runtime_system();
  CAMLreturn(pmask_to_ml(mask));
}

/* int numa_bitmask_equal(const struct bitmask * mask1, const struct bitmask * mask2)
 */
CAMLprim value caml_numa_bitmask_equal(value ml_mask1, value ml_mask2)
{
  CAMLparam2(ml_mask1, ml_mask2);
  int ret;
  struct bitmask * mask1;
  struct bitmask * mask2;

  mask1 = *((struct bitmask **)Data_custom_val(ml_mask1));
  mask2 = *((struct bitmask **)Data_custom_val(ml_mask2));

  caml_release_runtime_system();
  ret = numa_bitmask_equal(mask1, mask2);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* void copy_nodemask_to_bitmask(nodemask_t * nmask, struct bitmask * mask)
 */
CAMLprim value caml_copy_nodemask_to_bitmask(value ml_nmask, value ml_mask)
{
  CAMLparam2(ml_nmask, ml_mask);
  nodemask_t * nmask;
  struct bitmask * mask;

  nmask = *((nodemask_t **)Data_custom_val(ml_nmask));
  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  copy_nodemask_to_bitmask(nmask, mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* void copy_bitmask_to_nodemask(struct bitmask * mask, nodemask_t * nmask)
 */
CAMLprim value caml_copy_bitmask_to_nodemask(value ml_mask, value ml_nmask)
{
  CAMLparam2(ml_mask, ml_nmask);
  struct bitmask * mask;
  nodemask_t * nmask;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));
  nmask = *((nodemask_t **)Data_custom_val(ml_nmask));

  caml_release_runtime_system();
  copy_bitmask_to_nodemask(mask, nmask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* void copy_bitmask_to_bitmask(struct bitmask * bmpfrom, struct bitmask * bmpto)
 */
CAMLprim value caml_copy_bitmask_to_bitmask(value ml_bmpfrom, value ml_bmpto)
{
  CAMLparam2(ml_bmpfrom, ml_bmpto);
  struct bitmask * bmpfrom;
  struct bitmask * bmpto;

  bmpfrom = *((struct bitmask **)Data_custom_val(ml_bmpfrom));
  bmpto = *((struct bitmask **)Data_custom_val(ml_bmpto));

  caml_release_runtime_system();
  copy_bitmask_to_bitmask(bmpfrom, bmpto);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* void nodemask_zero(nodemask_t * mask)
 */
CAMLprim value caml_nodemask_zero(value ml_mask)
{
  CAMLparam1(ml_mask);
  nodemask_t * mask;

  mask = *((nodemask_t **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  nodemask_zero(mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* int nodemask_equal(const nodemask_t * a, const nodemask_t * b)
 */
CAMLprim value caml_nodemask_equal(value ml_a, value ml_b)
{
  CAMLparam2(ml_a, ml_b);
  int ret;
  nodemask_t * a;
  nodemask_t * b;

  a = *((nodemask_t **)Data_custom_val(ml_a));
  b = *((nodemask_t **)Data_custom_val(ml_b));

  caml_release_runtime_system();
  ret = nodemask_equal(a, b);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_available(void)
 */
CAMLprim value caml_numa_available(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_available();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_max_node(void)
 */
CAMLprim value caml_numa_max_node(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_max_node();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_max_possible_node(void)
 */
CAMLprim value caml_numa_max_possible_node(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_max_possible_node();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_preferred(void)
 */
CAMLprim value caml_numa_preferred(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_preferred();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* long long numa_node_size64(int node, long long * freep)
 */
CAMLprim value caml_numa_node_size64(value ml_node)
{
  CAMLparam1(ml_node);
  long long ret;
  int node;

  node = Int_val(ml_node);

  caml_release_runtime_system();
  ret = numa_node_size64(node, NULL);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* long numa_node_size(int node, long * freep)
 */
CAMLprim value caml_numa_node_size(value ml_node)
{
  CAMLparam1(ml_node);
  long ret;
  int node;

  node = Int_val(ml_node);

  caml_release_runtime_system();
  ret = numa_node_size(node, NULL);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_pagesize(void)
 */
CAMLprim value caml_numa_pagesize(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_pagesize();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* void numa_bind(struct bitmask * nodes)
 */
CAMLprim value caml_numa_bind(value ml_nodes)
{
  CAMLparam1(ml_nodes);
  struct bitmask * nodes;

  nodes = *((struct bitmask **)Data_custom_val(ml_nodes));

  caml_release_runtime_system();
  numa_bind(nodes);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* void numa_set_interleave_mask(struct bitmask * nodemask)
 */
CAMLprim value caml_numa_set_interleave_mask(value ml_nodemask)
{
  CAMLparam1(ml_nodemask);
  struct bitmask * nodemask;

  nodemask = *((struct bitmask **)Data_custom_val(ml_nodemask));

  numa_set_interleave_mask(nodemask);

  CAMLreturn(Val_unit);
}

/* struct bitmask * numa_get_interleave_mask(void)
 */
CAMLprim value caml_numa_get_interleave_mask(value unit __attribute__((unused)))
{
  CAMLparam0();
  struct bitmask * mask;

  caml_release_runtime_system();
  mask = numa_get_interleave_mask();
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

static void nodemask_finalize(value ml_value)
{
  struct bitmask **pmask;

  pmask = (struct bitmask **)Data_custom_val(ml_value);
  if (*pmask != NULL)
  {
    numa_free_nodemask(*pmask);
    *pmask = NULL;
  }
}

static struct custom_operations nodemask_custom_ops = {
  "nodemask",
  nodemask_finalize,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* struct bitmask * numa_allocate_nodemask(void)
 */
CAMLprim value caml_numa_allocate_nodemask(value unit __attribute__((unused)))
{
  CAMLparam0();
  CAMLlocal1(ml_mask);
  struct bitmask *mask, **pmask;

  mask = numa_allocate_nodemask();
  caml_release_runtime_system();
  ml_mask = caml_alloc_custom(&nodemask_custom_ops, sizeof mask, 0, 1);
  caml_acquire_runtime_system();
  pmask = (struct bitmask **)Data_custom_val(ml_mask);
  *pmask = mask;

  CAMLreturn(ml_mask);
}

/* void numa_set_preferred(int node)
 */
CAMLprim value caml_numa_set_preferred(value ml_node)
{
  CAMLparam1(ml_node);
  int node;

  node = Int_val(ml_node);

  caml_release_runtime_system();
  numa_set_preferred(node);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* void numa_set_localalloc(void)
 */
CAMLprim value caml_numa_set_localalloc(value unit __attribute__((unused)))
{
  caml_release_runtime_system();
  numa_set_localalloc();
  caml_acquire_runtime_system();

  return Val_unit;
}

/* void numa_set_membind(struct bitmask * nodemask)
 */
CAMLprim value caml_numa_set_membind(value ml_nodemask)
{
  CAMLparam1(ml_nodemask);
  struct bitmask * nodemask;

  nodemask = *((struct bitmask **)Data_custom_val(ml_nodemask));

  caml_release_runtime_system();
  numa_set_membind(nodemask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* struct bitmask * numa_get_membind(void)
 */
CAMLprim value caml_numa_get_membind(value unit __attribute__((unused)))
{
  CAMLparam0();
  CAMLlocal1(ml_ret);
  struct bitmask * mask;

  caml_release_runtime_system();
  mask = numa_get_membind();
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

/* struct bitmask * numa_get_mems_allowed(void)
 */
CAMLprim value caml_numa_get_mems_allowed(value unit __attribute__((unused)))
{
  CAMLparam0();
  CAMLlocal1(ml_ret);
  struct bitmask * mask;

  caml_release_runtime_system();
  mask = numa_get_mems_allowed();
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

/* int numa_get_interleave_node(void)
 */
CAMLprim value caml_numa_get_interleave_node(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_get_interleave_node();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_run_on_node_mask(struct bitmask * mask)
 */
CAMLprim value caml_numa_run_on_node_mask(value ml_mask)
{
  CAMLparam1(ml_mask);
  int ret;
  struct bitmask * mask;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  ret = numa_run_on_node_mask(mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_run_on_node_mask_all(struct bitmask * mask)
 */
CAMLprim value caml_numa_run_on_node_mask_all(value ml_mask)
{
  CAMLparam1(ml_mask);
  int ret;
  struct bitmask * mask;

  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  ret = numa_run_on_node_mask_all(mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_run_on_node(int node)
 */
CAMLprim value caml_numa_run_on_node(value ml_node)
{
  CAMLparam1(ml_node);
  int ret;
  int node;

  node = Int_val(ml_node);

  caml_release_runtime_system();
  ret = numa_run_on_node(node);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* struct bitmask * numa_get_run_node_mask(void)
 */
CAMLprim value caml_numa_get_run_node_mask(value unit __attribute__((unused)))
{
  CAMLparam0();
  CAMLlocal1(ml_ret);
  struct bitmask * mask;

  caml_release_runtime_system();
  mask = numa_get_run_node_mask();
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

/* void numa_set_bind_policy(int strict)
 */
CAMLprim value caml_numa_set_bind_policy(value ml_strict)
{
  CAMLparam1(ml_strict);
  int strict;

  strict = Int_val(ml_strict);

  caml_release_runtime_system();
  lib_mutex_lock();
  numa_set_bind_policy(strict);
  lib_mutex_unlock();
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* void numa_set_strict(int flag)
 */
CAMLprim value caml_numa_set_strict(value ml_flag)
{
  CAMLparam1(ml_flag);
  int flag;

  flag = Int_val(ml_flag);

  caml_release_runtime_system();
  numa_set_strict(flag);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

/* int numa_num_possible_nodes(void)
 */
CAMLprim value caml_numa_num_possible_nodes(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_num_possible_nodes();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_num_possible_cpus(void)
 */
CAMLprim value caml_numa_num_possible_cpus(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_num_possible_cpus();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_num_configured_nodes(void)
 */
CAMLprim value caml_numa_num_configured_nodes(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_num_configured_nodes();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_num_configured_cpus(void)
 */
CAMLprim value caml_numa_num_configured_cpus(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_num_configured_cpus();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_num_task_cpus(void)
 */
CAMLprim value caml_numa_num_task_cpus(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_num_task_cpus();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_num_task_nodes(void)
 */
CAMLprim value caml_numa_num_task_nodes(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_num_task_nodes();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_num_thread_nodes(void)
 */
CAMLprim value caml_numa_num_thread_nodes(value unit __attribute__((unused)))
{
  CAMLparam0();
  int ret;

  caml_release_runtime_system();
  ret = numa_num_thread_nodes();
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

static void cpumask_finalize(value ml_value)
{
  struct bitmask **pmask;

  pmask = (struct bitmask **)Data_custom_val(ml_value);
  if (*pmask != NULL)
  {
    numa_free_cpumask(*pmask);
    *pmask = NULL;
  }
}

static struct custom_operations cpumask_custom_ops = {
  "cpumask",
  cpumask_finalize,
  custom_compare_default,
  custom_compare_ext_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* struct bitmask * numa_allocate_cpumask(void)
 */
CAMLprim value caml_numa_allocate_cpumask(value unit __attribute__((unused)))
{
  CAMLparam0();
  CAMLlocal1(ml_mask);
  struct bitmask *mask, **pmask;

  mask = numa_allocate_cpumask();
  ml_mask = caml_alloc_custom(&cpumask_custom_ops, sizeof mask, 0, 1);
  caml_release_runtime_system();
  pmask = (struct bitmask **)Data_custom_val(ml_mask);
  caml_acquire_runtime_system();
  *pmask = mask;

  CAMLreturn(ml_mask);
}

/* int numa_node_to_cpus(int node, struct bitmask * mask)
 */
CAMLprim value caml_numa_node_to_cpus(value ml_node, value ml_mask)
{
  CAMLparam2(ml_node, ml_mask);
  int ret;
  int node;
  struct bitmask * mask;

  node = Int_val(ml_node);
  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  numa_bitmask_clearall(mask);
  ret = numa_node_to_cpus(node, mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_node_of_cpu(int cpu)
 */
CAMLprim value caml_numa_node_of_cpu(value ml_cpu)
{
  CAMLparam1(ml_cpu);
  int ret;
  int cpu;

  cpu = Int_val(ml_cpu);

  caml_release_runtime_system();
  ret = numa_node_of_cpu(cpu);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_distance(int node1, int node2)
 */
CAMLprim value caml_numa_distance(value ml_node1, value ml_node2)
{
  CAMLparam2(ml_node1, ml_node2);
  int ret;
  int node1;
  int node2;

  node1 = Int_val(ml_node1);
  node2 = Int_val(ml_node2);

  caml_release_runtime_system();
  ret = numa_distance(node1, node2);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_migrate_pages(int pid, struct bitmask * from, struct bitmask * to)
 */
CAMLprim value caml_numa_migrate_pages(value ml_pid, value ml_from, value ml_to)
{
  CAMLparam3(ml_pid, ml_from, ml_to);
  int ret;
  int pid;
  struct bitmask * from;
  struct bitmask * to;

  pid = Int_val(ml_pid);
  from = *((struct bitmask **)Data_custom_val(ml_from));
  to = *((struct bitmask **)Data_custom_val(ml_to));

  caml_release_runtime_system();
  ret = numa_migrate_pages(pid, from, to);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_sched_getaffinity(pid_t pid, struct bitmask * mask)
 */
CAMLprim value caml_numa_sched_getaffinity(value ml_pid, value ml_mask)
{
  CAMLparam2(ml_pid, ml_mask);
  int ret;
  pid_t pid;
  struct bitmask * mask;

  pid = Int_val(ml_pid);
  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  ret = numa_sched_getaffinity(pid, mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* int numa_sched_setaffinity(pid_t pid, struct bitmask * mask)
 */
CAMLprim value caml_numa_sched_setaffinity(value ml_pid, value ml_mask)
{
  CAMLparam2(ml_pid, ml_mask);
  int ret;
  pid_t pid;
  struct bitmask * mask;

  pid = Int_val(ml_pid);
  mask = *((struct bitmask **)Data_custom_val(ml_mask));

  caml_release_runtime_system();
  ret = numa_sched_setaffinity(pid, mask);
  caml_acquire_runtime_system();

  CAMLreturn(Val_int(ret));
}

/* struct bitmask * numa_parse_nodestring(const char * string)
 */
CAMLprim value caml_numa_parse_nodestring(value ml_string)
{
  CAMLparam1(ml_string);
  struct bitmask * mask;
  char * string;

  string = String_val(ml_string);

  caml_release_runtime_system();
  mask = numa_parse_nodestring(string);
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

/* struct bitmask * numa_parse_nodestring_all(const char * string)
 */
CAMLprim value caml_numa_parse_nodestring_all(value ml_string)
{
  CAMLparam1(ml_string);
  CAMLlocal1(ml_ret);
  struct bitmask * mask;
  char * string;

  string = String_val(ml_string);

  caml_release_runtime_system();
  mask = numa_parse_nodestring_all(string);
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

/* struct bitmask * numa_parse_cpustring(const char * string)
 */
CAMLprim value caml_numa_parse_cpustring(value ml_string)
{
  CAMLparam1(ml_string);
  CAMLlocal1(ml_ret);
  struct bitmask * mask;
  char * string;

  string = String_val(ml_string);

  caml_release_runtime_system();
  mask = numa_parse_cpustring(string);
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

/* struct bitmask * numa_parse_cpustring_all(const char * string)
 */
CAMLprim value caml_numa_parse_cpustring_all(value ml_string)
{
  CAMLparam1(ml_string);
  CAMLlocal1(ml_ret);
  struct bitmask * mask;
  char * string;

  string = String_val(ml_string);

  caml_release_runtime_system();
  mask = numa_parse_cpustring_all(string);
  caml_acquire_runtime_system();

  CAMLreturn(pmask_to_ml(mask));
}

