#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "scheme.h"

/*
  values are nan-boxed

  ieee 754 double precision floats
  sign | exp 11bits | fraction 52bits (most sig bit = quiet)

  all ones  in exponent     = special case
  all zeros in q + fraction = +- infinity
  any bits in fraction      = NaN

  quiet bit is about errors, just preserve it
  this is vaguely system dependent, but x86 and arm handle it this way

  rest available for signalling inside a NaN

  we need to preserve the ability to distinguish NaNs and infinities
  which requires that we _not_ have all zeros
  so type fields shouldn't use zeros

  additionally, let's use 48bit pointers 
  todo: this'll likely bite me in the ass for systems work

  64 bit systems are 48 bits address spaces
  52 - 48 = 4  possible bits for "native" pointer types
          = 15 choices (0 is unavailable due to distinguishablity)

  additonally, a pool of ids is set aside for _offsets_
  we can build object pools, to alleviate pointer hopping

  a pool is a (growable?) linear array of fixed size units
  a "reference" is a index to a pool, and an offset within
  pools can handle memory however they want to, but must have unique pool ids

  a handle is a type + pool_id + offset
  256 possible pool ids, this is more like page tables, than arbitrary makes

  todo: how can we represent an _actual_ 64byte int?
  definitely need (define flag 0xFFFFFFFFFFFFFFFF)

  we can do the same thing we do for cons pools
  a 64byte int (or arbitrary int maybe?) is a handle to an integer pool

                s | 52 bits (exponent bits are elided)

  +inf          0   00000000 .... 000000 00
  -inf          1   00000000 .... 000000 00
  NaN           0   00000000 .... 000000 01
  NaNq          0   10000000 .... 000000 01
                                          
  PTR           1   0ttt address
  malloc        1   0001 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  pool          1   0010 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  primitive     1   0011 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  etc.              0000 = -infinity / NaN
                    1000 = NaNq

                FFF      poolid           offset
  HND           1   1ttt pppppppppppppppp oooooooooooooooooooooooooooooooo
  cons pool     1   1000 pppppppppppppppp oooooooooooooooooooooooooooooooo
  symbol pool   1   1001 pppppppppppppppp oooooooooooooooooooooooooooooooo
  string pool   1   1010 pppppppppppppppp oooooooooooooooooooooooooooooooo
  etc.              0000 = -infinity / NaN
                    1000 = NaNq
  
  BOX           0   tttt aux              data
  boolean       0   0001 0000000000000000 0000000000000000000000000000000b
  character     0   0010 0000000000000000 000000000000000000000000cccccccc
  integer       0   0011 0000000000000000 dddddddddddddddddddddddddddddddd
  double        0   0100 0000000000000000 dddddddddddddddddddddddddddddddd
  error         0   1110 0000000000000000 dddddddddddddddddddddddddddddddd
  nil           0   1111 1111111111111111 11111111111111111111111111111111
  etc.              0000 = +inifinity / NaN
                    1000 = NaNq

  7  pointer types, 48 bit address
  7  handle types, 16bit pool id, 32bit offset
  14 box types, max 7 byte payload per type

  cons pool:
    linear array of value_t
    car cdr car cdr car cdr

  initialized with all nil (write all ones to the whole block)
*/

/* fixed known globals; extern'd in header */ 

#define CONS_POOL_SIZE     4096
#define STRING_BUFFER_SIZE 8192
#define SYMBOL_POOL_SIZE  24

value_t symbegin;
value_t symdefine;
value_t symif;
value_t symlambda;
value_t symquote;

// local forward decls

/* boxes */
inline static value_t    make_boxed(box_type_t type, uint16_t aux, box_data_t value);
inline static value_t    reshape_box(context_p ctxt, value_t v, box_type_t type);
inline static bool       is_boxed(box_type_t type, value_t v);
inline static box_type_t boxed_type(value_t v);
inline static uint16_t   boxed_aux(value_t v);
inline static box_data_t boxed_data(value_t v);

/* pointers */
inline static value_t    make_pointer(context_p ctxt, ptr_type_t type, void* addr);
inline static value_t    reshape_pointer(context_p ctxt, value_t v, ptr_type_t type);
inline static bool       is_pointer(ptr_type_t type, value_t v);
inline static ptr_type_t pointer_type(value_t v);
inline static void*      pointer_addr(value_t v);

/* handles */
inline static value_t    make_handle(context_p ctxt, hnd_type_t type, uint16_t aux, uint32_t offset);
inline static value_t    reshape_handle(context_p ctxt, value_t v, hnd_type_t type);
inline static bool       is_handle(hnd_type_t type, value_t v);
inline static hnd_type_t handle_type(value_t v);
inline static uint16_t   handle_aux(value_t v);
inline static uint32_t   handle_offset(value_t v);

/* pools and the ctxt */

context_p alloc_context(int initial_size) {
  context_p ctxt = malloc(sizeof(context_t));

  /* cons pool - initialized to all nil */
  int size      = initial_size * 2 * sizeof(value_t);
  value_t *pool = malloc(size);
  if (pool == NULL) {
    fprintf(stderr, "out of memory!\n");
    exit(1);
  }
  memset(pool, 0xFF, size);
  ctxt->cons_pool_size  = 1;
  ctxt->cons_pool_limit = initial_size;
  ctxt->cons_pool_ptr   = pool;
  ctxt->cons_free_list  = make_handle(ctxt, HND_CONS, 0, 0);

  /* symbol pool */
  size = SYMBOL_POOL_SIZE * sizeof(value_t);
  value_t *symbols = malloc(size);
  if (symbols == NULL) {
    fprintf(stderr, "out of memory\n");
    exit(1);
  }
  memset(symbols, 0xFF, size);
  ctxt->symbol_pool_size = SYMBOL_POOL_SIZE;
  ctxt->symbol_pool_ptr  = symbols;

  /* string buffer */
  size = initial_size;
  char *buffer = malloc(size);
  if (buffer == NULL) {
    fprintf(stderr, "out of memory!\n");
    exit(1);
  }
  memset(buffer, 0x00, size);
  ctxt->string_buffer_limit  = initial_size;
  ctxt->string_buffer_offset = 0;
  ctxt->string_buffer_ptr    = buffer;

  /* environments */
  ctxt->root_env = make_cons(ctxt, vnil, vnil);
  ctxt->curr_env = make_cons(ctxt, vnil, vnil);

  /* initialize known symbols */
  symbegin  = make_symbol(ctxt, "begin", 5);
  symdefine = make_symbol(ctxt, "define", 6);
  symif     = make_symbol(ctxt, "if", 2);
  symlambda = make_symbol(ctxt, "lambda", 6);
  symquote  = make_symbol(ctxt, "quote", 5);

  return ctxt;
}

static value_t alloc_cons(context_p ctxt, value_t car, value_t cdr) {
  int index = ctxt->cons_pool_size;
  
  ctxt->cons_pool_ptr[index]     = car;
  ctxt->cons_pool_ptr[index + 1] = cdr;
  ctxt->cons_pool_size          += 2;

  return make_handle(ctxt, HND_CONS, 0, index);
}

value_t environment_get(context_p ctxt, value_t env, value_t key) {
  value_t cursor = env;

  while(1) {
    if (is_nil(ctxt, cursor)) {
      return vnil;
    } 

    if (equality_exact(ctxt, cons_caar(ctxt, cursor), key)) {
      return cons_cdar(ctxt, cursor);
    }

    cursor = cons_cdr(ctxt, cursor);
  }
}

value_t environment_set(context_p ctxt, value_t env, value_t key, value_t value) {
  value_t newpair = make_cons(ctxt, key, value);
  return make_cons(ctxt, newpair, env);
}

/* comparisons */

/** exact value equality */
inline bool equality_exact(context_p, value_t a, value_t b) {
  return a.as_uint64 == b.as_uint64;
}

bool equality_string(context_p ctxt, value_t a, value_t b) {
  bool a_typed = is_string(ctxt, a) || is_symbol(ctxt, a);
  bool b_typed = is_string(ctxt, b) || is_symbol(ctxt, b);
  
  if (!a_typed || !b_typed) { return false; }
  if (string_len(ctxt, a) != string_len(ctxt, b)) { return false; }

  return strncmp(string_ptr(ctxt, a), string_ptr(ctxt, b), string_len(ctxt, a)) == 0;
}

bool equality_cstring(context_p ctxt, value_t a, char *value, uint32_t len) {
  bool a_typed = is_string(ctxt, a) || is_symbol(ctxt, a);
  if (a_typed && (string_len(ctxt, a) == len)) {
    return strncmp(string_ptr(ctxt, a), value, len) == 0;
  }

  return false;
}

inline bool is_boolean(context_p ctxt, value_t v) {
  return is_vtruth(ctxt, v) || is_vfalse(ctxt, v);
}

/** literal equality */
inline bool is_vtruth(context_p ctxt, value_t v) {
  return equality_exact(ctxt, v, vtrue);
}

/** literal equality */
inline bool is_vfalse(context_p ctxt, value_t v) {
  return equality_exact(ctxt, v, vfalse);
}

/** scheme-like predicate: everything but #f is truthy */
inline bool is_truthy(context_p ctxt, value_t v) {
  return !is_vfalse(ctxt, v);
}

/** scheme-like predicate: only #f is falsey */
inline bool is_falsey(context_p ctxt, value_t v) {
  return is_vfalse(ctxt, v);
}

/* doubles */

#define NOT_DOUBLE_MASK 0x7FF0000000000000
#define NOT_NANINF_MASK 0x0009000000000000

inline value_t make_double(context_p, double v) {
  return (value_t)v;
}

inline double as_double(context_p, value_t v) {
  return v.as_double;
}

inline bool is_double(context_p, value_t v) {
  uint64_t u = v.as_uint64;
  return (u & NOT_DOUBLE_MASK) != NOT_DOUBLE_MASK;
}

inline bool is_nan(context_p ctxt, value_t v) {
  return equality_exact(ctxt, v, vnan) || equality_exact(ctxt, v, vnanq);
}

inline bool is_inf(context_p ctxt, value_t v) {
  return equality_exact(ctxt, v, vpinf) || equality_exact(ctxt, v, vninf);
}

/* integers */

inline value_t make_integer(context_p, uint32_t value) {
  return make_boxed(BOX_INTEGER, 0, (box_data_t)value);
}

inline bool is_integer(context_p, value_t v) {
  return is_boxed(BOX_INTEGER, v);
}
  
inline uint32_t as_integer(context_p, value_t v) {
  return boxed_data(v).as_uint32;
}
/* floats */

inline value_t make_float(context_p, float value) {
  return make_boxed(BOX_FLOAT, 0, (box_data_t)value);
}

inline bool is_float(context_p, value_t v) {
  return is_boxed(BOX_FLOAT, v);
}

inline float as_float(context_p, value_t v) {
  return boxed_data(v).as_float;
}

/* chars */

inline value_t make_character(context_p, char value) {
  return make_boxed(BOX_CHARACTER, 0, (box_data_t)(uint32_t)value);
}

inline bool is_character(context_p, value_t v) {
  return is_boxed(BOX_CHARACTER, v);
}

inline char as_character(context_p, value_t v) {
  return (char)boxed_data(v).as_uint32;
}

inline value_t make_error(context_p, uint32_t code) {
  return make_boxed(BOX_ERROR, 0, (box_data_t)code);
}

inline bool is_error(context_p, value_t v) {
  return is_boxed(BOX_ERROR, v);
}

/* strings */

value_t make_string(context_p ctxt, char *str, int len) {
  int offset = ctxt->string_buffer_offset;
  memcpy(ctxt->string_buffer_ptr + offset, str, len);

  // should already be the case, but ensure
  ctxt->string_buffer_ptr[offset + len] = '\0';
  ctxt->string_buffer_offset += len;
  
  // TODO: error handling
  return make_handle(ctxt, HND_STRING, len, offset);
}

inline bool is_string(context_p, value_t v) {
  return is_handle(HND_STRING, v);
}

inline char* string_ptr(context_p ctxt, value_t v) {
  int offset = handle_offset(v);
  return ctxt->string_buffer_ptr + offset;
}

inline uint32_t string_len(context_p, value_t v) {
  return handle_aux(v);
}

/* symbols */

static int symbol_hash(char *name, unsigned int len) {
  unsigned int i;
  unsigned int hash = 0;
  for (i = 0; i < len; i++) {
    hash = (hash << 5) + name[i];
  }

  return hash % SYMBOL_POOL_SIZE;
}

value_t make_symbol(context_p ctxt, char* name, int len) {
  value_t pair, key;
  int hash = symbol_hash(name, len);

  pair = ctxt->symbol_pool_ptr[hash];

  // no list at the hash
  if (is_nil(ctxt, pair)) {
    key  = make_string(ctxt, name, len);
    key  = reshape_handle(ctxt, key, HND_SYMBOL);

    ctxt->symbol_pool_ptr[hash] = make_cons(ctxt, key, vnil);
    return key;
  }

  while (1) {
    value_t car = cons_car(ctxt, pair);
    value_t cdr = cons_cdr(ctxt, pair);
    
    // found it
    if (is_symbol(ctxt, car) &&
        equality_cstring(ctxt, car, name, len)) {
      return car;
    }

    // not in the list, add to the bottom
    if (is_nil(ctxt, cdr)) {
      key = make_string(ctxt, name, len);
      key = reshape_handle(ctxt, key, HND_SYMBOL);
      
      car = make_cons(ctxt, key, vnil);
      cons_set_cdr(ctxt, pair, car);

      return key;
    }

    // keep looking
    pair = cons_cdr(ctxt, pair);
  }
}

inline bool is_symbol(context_p, value_t v) {
  return is_handle(HND_SYMBOL, v);
}

/* errors */

/* cons cells */

inline value_t make_cons(context_p ctxt, value_t car, value_t cdr) {
  return alloc_cons(ctxt, car, cdr);
}

inline bool is_cons(context_p, value_t v) {
  return is_handle(HND_CONS, v);
}

inline bool is_atom(context_p ctxt, value_t v) {
  return !is_cons(ctxt, v);
}

inline bool is_nil(context_p ctxt, value_t v) {
  return equality_exact(ctxt, v, vnil);
}

inline value_t cons_car(context_p ctxt, value_t hnd) {
  int index = handle_offset(hnd);
  return ctxt->cons_pool_ptr[index];
}

inline value_t cons_cdr(context_p ctxt, value_t hnd) {
  int index = handle_offset(hnd);
  return ctxt->cons_pool_ptr[index + 1];
}

inline void cons_set_car(context_p ctxt, value_t hnd, value_t v) {
  int index = handle_offset(hnd);
  ctxt->cons_pool_ptr[index] = v;
}

inline void cons_set_cdr(context_p ctxt, value_t hnd, value_t v) {
  int index = handle_offset(hnd);
  ctxt->cons_pool_ptr[index + 1] = v;
}

/* buffers */

/* vectors */

value_t make_vector(context_p ctxt, int size, value_t fill) {
  size += 1; // account for size header
  value_t *vecptr = malloc((size + 1) * sizeof(value_t));
  if (vecptr == NULL) {
    fprintf(stderr, "out of memory!\n");
    exit(1);
  }

  vecptr[0] = make_integer(ctxt, size);
  for (; size > 0; size--) {
    vecptr[size + 1] = fill;
  }

  return make_pointer(ctxt, PTR_VECTOR, vecptr);
}

inline bool is_vector(context_p, value_t v) {
  return is_pointer(PTR_VECTOR, v);
}

inline value_t vector_get(context_p, value_t v, int index) {
  // todo: bounds check
  return ((value_t*)pointer_addr(v))[index];
}

inline void vector_set(context_p, value_t v, int index, value_t val) {
  // todo: bounds check
  ((value_t*)pointer_addr(v))[index] = val;
}

inline value_t vector_size(context_p, value_t v) {
  return ((value_t*)pointer_addr(v))[0];
}

/* procs */

inline value_t make_native_proc(context_p ctxt, native_proc_fn fn) {
  return make_pointer(ctxt, PTR_NATIVE_PROC, fn);
}

inline bool is_native_proc(context_p, value_t v) {
  return is_pointer(PTR_NATIVE_PROC, v);
}

inline native_proc_fn native_proc_function(context_p, value_t v) {
  return (native_proc_fn)pointer_addr(v);
}

// captures env!
// (body . (args . env))
inline value_t make_compound_proc(context_p ctxt, value_t args, value_t body, value_t env) {
  value_t v = make_cons(ctxt, body, make_cons(ctxt, args, env));
  return reshape_handle(ctxt, v, HND_PROC);
}

inline value_t compound_proc_body(context_p ctxt, value_t v) {
  return cons_car(ctxt, v);
}

inline value_t compound_proc_args(context_p ctxt, value_t v) {
  return cons_cadr(ctxt, v);
}

inline value_t compound_proc_env(context_p ctxt, value_t v) {
  return cons_cddr(ctxt, v);
}

inline bool is_compound_proc(context_p, value_t v) {
  return is_handle(HND_PROC, v);
}

inline bool is_proc(context_p ctxt, value_t v) {
  return is_compound_proc(ctxt, v) || is_native_proc(ctxt, v);
}

/* conversions */

value_t to_integer(context_p ctxt, value_t v) {
  if (is_character(ctxt, v)) {
    return reshape_box(ctxt, v, BOX_INTEGER);
  }

  if (is_double(ctxt, v)) {
    return make_integer(ctxt, round(as_double(ctxt, v)));
  }

  if (is_string(ctxt, v)) {
    uint32_t num = atoi(string_ptr(ctxt, v));
    return make_integer(ctxt, num);
  }

  return make_error(ctxt, __LINE__);
}

value_t to_character(context_p ctxt, value_t v) {
  if (is_integer(ctxt, v) && (as_integer(ctxt, v) < 256)) {
    return reshape_box(ctxt, v, BOX_CHARACTER);
  }

  return make_error(ctxt, __LINE__);
}

value_t to_string(context_p ctxt, value_t v) {
  if (is_symbol(ctxt, v)) {
    value_t sym = environment_get(ctxt, v, ctxt->curr_env);
    return reshape_handle(ctxt, sym, HND_STRING);
  }

  if (is_integer(ctxt, v)) {
    char buffer[256];
    int len = sprintf(buffer, "%d", as_integer(ctxt, v));
    return make_string(ctxt, buffer, len);
  }

  if (is_double(ctxt, v)) {
    char buffer[256];
    int len = sprintf(buffer, "%lf", as_double(ctxt, v));
    return make_string(ctxt, buffer, len);
  }

  return make_error(ctxt, __LINE__);
}

value_t to_symbol(context_p ctxt, value_t v) {
  if (is_string(ctxt, v)) {
    return make_symbol(ctxt, string_ptr(ctxt, v), string_len(ctxt, v));
  }

  return make_error(ctxt, __LINE__);
}

/* boxes */

#define SPECIAL_MASK    0xFFF0000000000000
#define BOX_MASK        0x7FF0000000000000
#define BOX_TYPE_MASK   0x000F000000000000
#define BOX_AUX_MASK    0x0000FFFF00000000
#define BOX_DATA_MASK   0x00000000FFFFFFFF

static value_t make_boxed(box_type_t type, uint16_t aux, box_data_t value) {
  value_t v;
  v.as_uint64 = BOX_MASK   |
    ((uint64_t)type << 48) |
    ((uint64_t)aux << 32)  |
    ((uint64_t)value.as_uint32);

  return v;
}

inline value_t reshape_box(context_p, value_t v, box_type_t type) {
  uint64_t typeless  = v.as_uint64 & ~BOX_TYPE_MASK;
  uint64_t type_mask = (uint64_t)type << 48;

  return (value_t)(typeless | type_mask);
}

inline static bool is_boxed(box_type_t type, value_t v) {
  uint64_t type_mask = (uint64_t)type << 48;
  return (v.as_uint64 & (SPECIAL_MASK | BOX_TYPE_MASK)) == (BOX_MASK | type_mask);
}

inline static box_type_t boxed_type(value_t v) {
  return ((box_type_t)((v.as_uint64 & BOX_TYPE_MASK) >> 48));
}

inline static uint16_t boxed_aux(value_t v) {
  return ((uint16_t)((v.as_uint64 & BOX_AUX_MASK) >> 32));
}

inline static box_data_t boxed_data(value_t v) {
  return (box_data_t)((uint32_t)(v.as_uint64 & BOX_DATA_MASK));
}

/* arbitrary pointers */

#define PTR_MASK        0xFFF0000000000000
#define PTR_TYPE_MASK   0x000F000000000000
#define PTR_ADDR_MASK   0x0000FFFFFFFFFFFF

static value_t make_pointer(context_p, ptr_type_t type, void* addr) {
  uint64_t as_int = (uint64_t)addr;

  if ((as_int & PTR_ADDR_MASK) != as_int) {
    fprintf(stderr, "unrepresentable address: %llx\nbailing!\n", as_int);
    exit(1);
  }
  
  value_t v;
  v.as_uint64 = PTR_MASK | (as_int & PTR_ADDR_MASK) | ((uint64_t)type << 48);

  return v;
}

inline static bool is_pointer(ptr_type_t type, value_t v) {
  uint64_t type_mask = (uint64_t)type << 48;
  return (v.as_uint64 & (PTR_MASK | PTR_TYPE_MASK)) == (PTR_MASK | type_mask);
}

inline static value_t reshape_pointer(context_p, value_t v, ptr_type_t type) {
  uint64_t typeless  = v.as_uint64 & ~PTR_TYPE_MASK;
  uint64_t type_mask = (uint64_t)type << 48;

  return (value_t)(typeless | type_mask);
}

inline static ptr_type_t pointer_type(value_t v) {
  return ((ptr_type_t)((v.as_uint64 & PTR_TYPE_MASK) >> 48));
}

inline static void* pointer_addr(value_t v) {
  return (void *)(v.as_uint64 & PTR_ADDR_MASK);
}

/* handles */

#define HND_AUX_MASK   0x0000FFFF00000000
#define HND_OFFSET_MASK 0x00000000FFFFFFFF

inline static value_t make_handle(context_p, hnd_type_t type, uint16_t aux, uint32_t offset) {
  value_t v;
  v.as_uint64 = PTR_MASK | offset | ((uint64_t)type << 48) | ((uint64_t)aux << 32);

  return v;
}

inline static value_t reshape_handle(context_p, value_t v, hnd_type_t type) {
  uint64_t typeless  = v.as_uint64 & ~PTR_TYPE_MASK;
  uint64_t type_mask = (uint64_t)type << 48;

  return (value_t)(typeless | type_mask);
}

inline static bool is_handle(hnd_type_t type, value_t v) {
  uint64_t type_mask = (uint64_t)type << 48;
  return (v.as_uint64 & (PTR_MASK | PTR_TYPE_MASK)) == (PTR_MASK | type_mask);
}

inline static hnd_type_t handle_type(value_t v) {
  return ((hnd_type_t)((v.as_uint64 & PTR_TYPE_MASK) >> 48));
}

inline static uint16_t handle_aux(value_t v) {
  return ((uint16_t)((v.as_uint64 & HND_AUX_MASK) >> 32));
}

inline static uint32_t handle_offset(value_t v) {
  return (uint32_t)(v.as_uint64 & HND_OFFSET_MASK);
}
