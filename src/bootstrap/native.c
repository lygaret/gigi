#include <string.h>
#include "scheme.h"

static value_t install_op(context_p ctxt, value_t env, char *name, native_proc_fn fn) {
  value_t sym = make_symbol(ctxt, name, strlen(name));
  value_t ptr = make_native_proc(ctxt, fn);
  
  return environment_set(ctxt, env, sym, ptr);
}

static value_t debugprint_proc(context_p ctxt, value_t args, value_t env) {
  value_t v = eval(ctxt, cons_car(ctxt, args), &env);
  printf("[%lx] => ", v.as_uint64);
  print(ctxt, v);
  printf("\n");

  return vnil;
}

static value_t nullp_proc(context_p ctxt, value_t args, value_t env) {
  return is_nil(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t eqp_proc(context_p ctxt, value_t args, value_t env) {
  value_t a = eval(ctxt, cons_car(ctxt, args), &env);
  value_t b = eval(ctxt, cons_cadr(ctxt, args), &env);

  return equality_exact(ctxt, a, b) ? vtrue : vfalse;
}

static value_t boolp_proc(context_p ctxt, value_t args, value_t env) {
  return is_boolean(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t symbolp_proc(context_p ctxt, value_t args, value_t env) {
  return is_symbol(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t integerp_proc(context_p ctxt, value_t args, value_t env) {
  return is_integer(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t floatp_proc(context_p ctxt, value_t args, value_t env) {
  return is_float(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t charp_proc(context_p ctxt, value_t args, value_t env) {
  return is_character(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t stringp_proc(context_p ctxt, value_t args, value_t env) {
  return is_string(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t consp_proc(context_p ctxt, value_t args, value_t env) {
  return is_cons(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t procp_proc(context_p ctxt, value_t args, value_t env) {
  return is_proc(ctxt, eval(ctxt, cons_car(ctxt, args), &env)) ? vtrue : vfalse;
}

static value_t to_integer_proc(context_p ctxt, value_t args, value_t env) {
  return to_integer(ctxt, eval(ctxt, cons_car(ctxt, args), &env));
}

static value_t to_character_proc(context_p ctxt, value_t args, value_t env) {
  return to_character(ctxt, eval(ctxt, cons_car(ctxt, args), &env));
}

static value_t to_string_proc(context_p ctxt, value_t args, value_t env) {
  return to_string(ctxt, eval(ctxt, cons_car(ctxt, args), &env));
}

static value_t to_symbol_proc(context_p ctxt, value_t args, value_t env) {
  return to_symbol(ctxt, eval(ctxt, cons_car(ctxt, args), &env));
}

static value_t intadd_proc(context_p ctxt, value_t args, value_t env) {
  value_t  cursor = args;
  uint32_t sum    = 0;
  value_t  car;

  while (!is_nil(ctxt, cursor)) {
    car    = eval(ctxt, cons_car(ctxt, cursor), &env);
    sum   += as_integer(ctxt, car);
    cursor = cons_cdr(ctxt, cursor);
  }

  return make_integer(ctxt, sum);
}

static value_t intsub_proc(context_p ctxt, value_t args, value_t env) {
  value_t  cursor = args;
  uint32_t diff   = 0;
  value_t  car    = eval(ctxt, cons_car(ctxt, cursor), &env);

  // unary minus, negate
  if (is_nil(ctxt, cons_cadr(ctxt, cursor))) {
    return make_integer(ctxt, 0 - as_integer(ctxt, car));
  }

  diff   = as_integer(ctxt, car);
  cursor = cons_cdr(ctxt, cursor);
  while (!is_nil(ctxt, cursor)) {
    car    = eval(ctxt, cons_car(ctxt, cursor), &env);
    diff  -= as_integer(ctxt, car);

    cursor = cons_cdr(ctxt, cursor);
  }

  return make_integer(ctxt, diff);
}

static value_t intmul_proc(context_p ctxt, value_t args, value_t env) {
  value_t  cursor = args;
  uint32_t total  = 1;
  value_t  car;

  while (!is_nil(ctxt, cursor)) {
    car    = eval(ctxt, cons_car(ctxt, cursor), &env);
    total *= as_integer(ctxt, car);
    cursor = cons_cdr(ctxt, cursor);
  }

  return make_integer(ctxt, total);
}

static value_t compgt_proc(context_p ctxt, value_t args, value_t env) {
  value_t  cursor = args;
  value_t  car, cadr;

  car = eval(ctxt, cons_car(ctxt, cursor), &env);
  while (!is_nil(ctxt, cursor)) {
    cadr = eval(ctxt, cons_cadr(ctxt, cursor), &env);

    bool test = as_integer(ctxt, car) > as_integer(ctxt, cadr);
    if (!test) {
      return vfalse;
    }

    car    = cadr;
    cursor = cons_cdr(ctxt, cursor);
  }

  return vtrue;
}

static value_t compgte_proc(context_p ctxt, value_t args, value_t env) {
  value_t  cursor = args;
  value_t  car, cadr;

  car = eval(ctxt, cons_car(ctxt, cursor), &env);
  while (!is_nil(ctxt, cursor)) {
    cadr = eval(ctxt, cons_cadr(ctxt, cursor), &env);

    bool test = as_integer(ctxt, car) >= as_integer(ctxt, cadr);
    if (!test) {
      return vfalse;
    }

    car    = cadr;
    cursor = cons_cdr(ctxt, cursor);
  }

  return vtrue;
}

static value_t complte_proc(context_p ctxt, value_t args, value_t env) {
  value_t  cursor = args;
  value_t  car, cadr;

  car = eval(ctxt, cons_car(ctxt, cursor), &env);
  while (!is_nil(ctxt, cursor)) {
    cadr = eval(ctxt, cons_cadr(ctxt, cursor), &env);

    bool test = as_integer(ctxt, car) <= as_integer(ctxt, cadr);
    if (!test) {
      return vfalse;
    }

    car    = cadr;
    cursor = cons_cdr(ctxt, cursor);
  }

  return vtrue;
}

static value_t complt_proc(context_p ctxt, value_t args, value_t env) {
  value_t  cursor = args;
  value_t  car, cadr;

  car = eval(ctxt, cons_car(ctxt, cursor), &env);
  while (!is_nil(ctxt, cursor)) {
    cadr = eval(ctxt, cons_cadr(ctxt, cursor), &env);

    bool test = as_integer(ctxt, car) < as_integer(ctxt, cadr);
    if (!test) {
      return vfalse;
    }

    car    = cadr;
    cursor = cons_cdr(ctxt, cursor);
  }

  return vtrue;
}

static value_t cons_proc(context_p ctxt, value_t args, value_t env) {
  value_t car = eval(ctxt, cons_car(ctxt, args), &env);
  value_t cdr = eval(ctxt, cons_cadr(ctxt, args), &env);

  return make_cons(ctxt, car, cdr);
}

static value_t car_proc(context_p ctxt, value_t args, value_t env) {
  value_t cons = eval(ctxt, cons_car(ctxt, args), &env);
  return cons_car(ctxt, cons);
}

static value_t cdr_proc(context_p ctxt, value_t args, value_t env) {
  value_t cons = eval(ctxt, cons_car(ctxt, args), &env);
  return cons_cdr(ctxt, cons);
}

static value_t car_set_proc(context_p ctxt, value_t args, value_t env) {
  value_t cons = eval(ctxt, cons_car(ctxt, args), &env);
  value_t val  = eval(ctxt, cons_cadr(ctxt, args), &env);

  cons_set_car(ctxt, cons, val);
  return vnil;
}

static value_t cdr_set_proc(context_p ctxt, value_t args, value_t env) {
  value_t cons = eval(ctxt, cons_car(ctxt, args), &env);
  value_t val  = eval(ctxt, cons_cadr(ctxt, args), &env);

  cons_set_cdr(ctxt, cons, val);
  return vnil;
}

static value_t list_proc(context_p ctxt, value_t args, value_t env) {
  value_t cursor = args;
  value_t car, cdr;
  
  while(!is_nil(ctxt, cursor)) {
    car = eval(ctxt, cons_car(ctxt, cursor), &env);
    cons_set_car(ctxt, cursor, car);
    
    cursor = cons_cdr(ctxt, cursor);
  }

  return args;
}

static value_t not_implemented_proc(context_p, value_t, value_t) {
  fprintf(stderr, "no implemented, sorry\n");
  return vnil;
}

value_t enhance_native_environment(context_p ctxt) {
  value_t env = ctxt->curr_env;

  env = install_op(ctxt, env, "print-debug",    &debugprint_proc);

  env = install_op(ctxt, env, "null?",          &nullp_proc);
  env = install_op(ctxt, env, "eq?",            &eqp_proc);

  env = install_op(ctxt, env, "bool?",          &boolp_proc);
  env = install_op(ctxt, env, "symbol?",        &symbolp_proc);
  env = install_op(ctxt, env, "integer?",       &integerp_proc);
  env = install_op(ctxt, env, "float?",         &floatp_proc);
  env = install_op(ctxt, env, "char?",          &charp_proc);
  env = install_op(ctxt, env, "string?",        &stringp_proc);
  env = install_op(ctxt, env, "pair?",          &consp_proc);
  env = install_op(ctxt, env, "procedure?",     &procp_proc);

  env = install_op(ctxt, env, "char->integer",  &to_integer_proc);
  env = install_op(ctxt, env, "integer->char",  &to_character_proc);
  env = install_op(ctxt, env, "number->string", &to_string_proc);
  env = install_op(ctxt, env, "string->number", &to_integer_proc);
  env = install_op(ctxt, env, "symbol->string", &to_string_proc);
  env = install_op(ctxt, env, "string->symbol", &to_symbol_proc);

  env = install_op(ctxt, env, "+",              &intadd_proc);
  env = install_op(ctxt, env, "-",              &intsub_proc);
  env = install_op(ctxt, env, "*",              &intmul_proc);
  env = install_op(ctxt, env, "quotient",       &not_implemented_proc);
  env = install_op(ctxt, env, "remainder",      &not_implemented_proc);
  env = install_op(ctxt, env, "=",              &eqp_proc);
  env = install_op(ctxt, env, "<",              &compgt_proc);
  env = install_op(ctxt, env, ">",              &complt_proc);
  env = install_op(ctxt, env, ">=",             &compgte_proc);
  env = install_op(ctxt, env, "<=",             &complte_proc);

  env = install_op(ctxt, env, "cons",           &cons_proc);
  env = install_op(ctxt, env, "car",            &car_proc);
  env = install_op(ctxt, env, "cdr",            &cdr_proc);
  env = install_op(ctxt, env, "set-car!",       &car_set_proc);
  env = install_op(ctxt, env, "set-cdr!",       &cdr_set_proc);
  env = install_op(ctxt, env, "list",           &list_proc);

  return env;
}
