#ifndef __scheme_h
#define __scheme_h

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#define unused(v) ((void)v);

typedef union value {
  double   as_double;
  uint64_t as_uint64;
} value_t;

typedef struct context {
  int cons_pool_size;
  int cons_pool_limit;
  value_t *cons_pool_ptr;
  value_t cons_free_list;
  int symbol_pool_size;
  value_t *symbol_pool_ptr;
  int string_buffer_limit;
  int string_buffer_offset;
  char *string_buffer_ptr;
  value_t root_env;
  value_t curr_env;
} context_t;

typedef context_t* context_p;

typedef enum {
  PTR_INVALID = 0,
  PTR_MALLOC,
  PTR_VECTOR,
  PTR_NATIVE_PROC,
} ptr_type_t;

typedef enum {
  HND_INVALID = 8,
  HND_CONS,
  HND_SYMBOL,
  HND_STRING,
  HND_PROC,
} hnd_type_t;

typedef enum {
  BOX_INVALID = 0,
  BOX_BOOLEAN,
  BOX_CHARACTER,
  BOX_INTEGER,
  BOX_FLOAT,

  BOX_ERROR = 0xE,
  BOX_NIL   = 0xF
} box_type_t;

/* convenience to avoid casting */
typedef union box_data {
  float    as_float;
  int32_t  as_int32;
  uint32_t as_uint32;
} box_data_t;

context_p alloc_context(int initial_size);
value_t   enhance_native_environment(context_p ctxt);
value_t   enhance_scheme_environment(context_p ctxt);

#define vnan   ((value_t)((uint64_t)0x7FF0000000000001LL))
#define vnanq  ((value_t)((uint64_t)0x7FF1000000000001LL))
#define vpinf  ((value_t)((uint64_t)0x7FF0000000000000LL))
#define vninf  ((value_t)((uint64_t)0xFFF0000000000000LL))

#define vnil   ((value_t)((uint64_t)0xFFFFFFFFFFFFFFFFLL))
#define vtrue  ((value_t)((uint64_t)0x7FF1000000000001LL))
#define vfalse ((value_t)((uint64_t)0x7FF1000000000000LL))

extern value_t symbegin;
extern value_t symdefine;
extern value_t symif;
extern value_t symlambda;
extern value_t symquote;

/* the machine */
context_p  alloc_context(int);

value_t    read(context_p, FILE*);
value_t    eval(context_p, value_t v, value_t *inoutenv);
void       print(context_p, value_t);

value_t    environment_get(context_p, value_t env, value_t key);
value_t    environment_set(context_p, value_t env, value_t key, value_t val);

/* conversions */
value_t    to_integer(context_p, value_t);
value_t    to_character(context_p, value_t);
value_t    to_string(context_p, value_t);
value_t    to_symbol(context_p, value_t);

/* comparisons */
bool       equality_exact(context_p, value_t, value_t);
bool       equality_string(context_p, value_t, value_t);
bool       equality_cstring(context_p, value_t str, char* val, uint32_t len);

/* refs (both pointers & handles) */
bool       is_reference(context_p, value_t);

/* bools */
bool       is_boolean(context_p, value_t);
bool       is_vtruth(context_p, value_t);
bool       is_vfalse(context_p, value_t);

/* schemey bools (false = #f, everything else = #t) */
bool       is_truthy(context_p, value_t);
bool       is_falsey(context_p, value_t);

/* doubles */
value_t    make_double(context_p, double);
double     as_double(context_p, value_t);
bool       is_double(context_p, value_t);
bool       is_nan(context_p, value_t);
bool       is_inf(context_p, value_t);

/* ints */
value_t    make_integer(context_p, uint32_t);
bool       is_integer(context_p, value_t);
uint32_t   as_integer(context_p, value_t);

/* floats */
value_t    make_float(context_p, float);
bool       is_float(context_p, value_t);
float      as_float(context_p, value_t);

/* chars */
value_t    make_character(context_p, char);
bool       is_character(context_p, value_t);
char       as_character(context_p, value_t);

/* strings */
value_t    make_string(context_p, char*, int);
bool       is_string(context_p, value_t);
char*      string_ptr(context_p, value_t);
uint32_t   string_len(context_p, value_t);

/* symbols */
value_t    make_symbol(context_p, char *name, int len);
bool       is_symbol(context_p, value_t);

/* errors */
value_t    make_error(context_p, uint32_t code);
bool       is_error(context_p, value_t);

/* cons cells */
value_t    make_cons(context_p, value_t car, value_t cdr);
bool       is_cons(context_p, value_t);
bool       is_atom(context_p, value_t);
bool       is_nil(context_p, value_t);

value_t    cons_car(context_p, value_t);
value_t    cons_cdr(context_p, value_t);
#define cons_caar(ctxt, hnd)   cons_car(ctxt, cons_car(ctxt, hnd))
#define cons_cadr(ctxt, hnd)   cons_car(ctxt, cons_cdr(ctxt, hnd))
#define cons_cdar(ctxt, hnd)   cons_cdr(ctxt, cons_car(ctxt, hnd))
#define cons_cddr(ctxt, hnd)   cons_cdr(ctxt, cons_cdr(ctxt, hnd))
#define cons_cdddr(ctxt, hnd)  cons_cdr(ctxt, cons_cddr(ctxt, hnd))
#define cons_caddr(ctxt, hnd)  cons_car(ctxt, cons_cddr(ctxt, hnd))
#define cons_cadddr(ctxt, hnd) cons_car(ctxt, cons_cdddr(ctxt, hnd))

void       cons_set_car(context_p, value_t pair, value_t val);
void       cons_set_cdr(context_p, value_t pair, value_t val);
#define cons_set_caar(ctxt, hnd, v) cons_set_car(ctxt, cons_car(ctxt, hnd), v)
#define cons_set_cadr(ctxt, hnd, v) cons_set_car(ctxt, cons_cdr(ctxt, hnd), v)
#define cons_set_cdar(ctxt, hnd, v) cons_set_cdr(ctxt, cons_car(ctxt, hnd), v)
#define cons_set_cddr(ctxt, hnd, v) cons_set_cdr(ctxt, cons_cdr(ctxt, hnd), v)

/* buffers */
value_t    make_buffer(context_p, int size, char fill);
bool       is_buffer(context_p, value_t v);

/* vectors */
value_t    make_vector(context_p, int size, value_t fill);
bool       is_vector(context_p, value_t v);

/* procs */

typedef value_t (*native_proc_fn)(context_p, value_t args, value_t env);

bool       is_proc(context_p ctxt, value_t v);

value_t    make_compound_proc(context_p, value_t args, value_t body, value_t env);
bool       is_compound_proc(context_p, value_t v);
value_t    compound_proc_body(context_p, value_t v);
value_t    compound_proc_args(context_p, value_t v);
value_t    compound_proc_env(context_p, value_t v);

value_t    make_native_proc(context_p, native_proc_fn fn);
bool       is_native_proc(context_p, value_t v);

native_proc_fn native_proc_function(context_p, value_t v);

#endif
