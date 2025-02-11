#include <stdio.h>
#include "scheme.h"

value_t eval(context_p ctxt, value_t v, value_t* env) {
 tailcall:
  if (is_nil(ctxt, v)) {
    return v;
  }

  /* atoms */
  if (is_atom(ctxt, v)) {
    if (is_symbol(ctxt, v)) {
      return environment_get(ctxt, *env, v);
    }

    if (is_integer(ctxt, v))   { return v; }
    if (is_character(ctxt, v)) { return v; }
    if (is_float(ctxt, v))     { return v; }
    if (is_double(ctxt, v))    { return v; }
    if (is_string(ctxt, v))    { return v; }
    if (is_boolean(ctxt, v))   { return v; }

    fprintf(stderr, "this object isn't handled currently\n");
    return vnil;
  }

  /* pairs */
  else {
    value_t car = cons_car(ctxt, v);

    // (quote ...)
    if (equality_exact(ctxt, symquote, car)) {
      return cons_cadr(ctxt, v);
    }

    // (if ...)
    if (equality_exact(ctxt, symif, car)) {
      value_t test = eval(ctxt, cons_cadr(ctxt, v), env);

      v = is_truthy(ctxt, test)
        ? cons_caddr(ctxt, v)
        : cons_cadddr(ctxt, v);
      goto tailcall;
    }

    // (define ...)
    if (equality_exact(ctxt, symdefine, car)) {
      value_t val = eval(ctxt, cons_caddr(ctxt, v), env);
      *env = environment_set(ctxt, *env, cons_cadr(ctxt, v), val);

      // include the function in it's own captured env
      // (body . (args . env))
      if (is_compound_proc(ctxt, val)) {
        cons_set_cdr(ctxt, cons_cdr(ctxt, val), *env);
      }

      return vnil;
    }

    // (begin ...)
    if (equality_exact(ctxt, symbegin, car)) {
      value_t cursor = cons_cdr(ctxt, v);
      value_t car, cdr;
      
      while (1) {
        car = cons_car(ctxt, cursor);
        cdr = cons_cdr(ctxt, cursor);

        if (is_nil(ctxt, cdr)) {
          v = car;
          goto tailcall;
        }

        eval(ctxt, car, env);
        cursor = cdr;
      }
    }

    // (lambda (vars) body...)
    if (equality_exact(ctxt, symlambda, car)) {
      value_t args, body;

      args = cons_cadr(ctxt, v);
      body = make_cons(ctxt, symbegin, cons_cddr(ctxt, v));
      return make_compound_proc(ctxt, args, body, *env);
    }

    // otherwise eval the car and invoke it
    car = eval(ctxt, car, env);

    if (is_compound_proc(ctxt, car)) {
      value_t args   = cons_cdr(ctxt, v);
      value_t params = compound_proc_args(ctxt, car);
      value_t body   = compound_proc_body(ctxt, car);
      value_t capenv = compound_proc_env(ctxt, car);

      // bind all the args in a new environment
      while (!is_nil(ctxt, params) && !is_nil(ctxt, args)) {
        value_t arg = eval(ctxt, cons_car(ctxt, args), env);
        capenv = environment_set(ctxt, capenv, cons_car(ctxt, params), arg);

        params = cons_cdr(ctxt, params);
        args   = cons_cdr(ctxt, args);
      }

      // eval the body in the new environment
      v = body;
      env = &capenv;
      goto tailcall;
    }

    if (is_native_proc(ctxt, car)) {
      native_proc_fn fn = native_proc_function(ctxt, car);
      return (*fn)(ctxt, cons_cdr(ctxt, v), *env);
    }

    printf("not a function!\n");
    return vnil;
  }

}
