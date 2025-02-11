#include <stdio.h>
#include <float.h>
#include "scheme.h"

static void print_cons(context_p ctxt, value_t v);

void print(context_p ctxt, value_t v) {
  if (is_nil(ctxt, v)) {
    printf("()");
    return;
  }

  if (is_vtruth(ctxt, v)) {
    printf("#t");
    return;
  }

  if (is_vfalse(ctxt, v)) {
    printf("#f");
    return;
  }

  if (is_compound_proc(ctxt, v)) {
    printf("#<proc:user>");
    return;
  }

  if (is_native_proc(ctxt, v)) {
    printf("#<proc:native>");
    return;
  }

  if (is_cons(ctxt, v)) {
    printf("(");
    print_cons(ctxt, v);
    return;
  }

  if (is_string(ctxt, v)) {
    int len = string_len(ctxt, v);
    char *ptr = string_ptr(ctxt, v);
    printf("\"%.*s\"", len, ptr);
    return;
  }

  if (is_symbol(ctxt, v)) {
    int len = string_len(ctxt, v);
    char *ptr = string_ptr(ctxt, v);
    printf("%.*s", len, ptr);
    return;
  }

  if (is_character(ctxt, v)) {
    char value;
    
    switch((value = as_character(ctxt, v))) {
    case ' ':
      printf("\\space");
      return;
    case '\n':
      printf("\\newline");
      return; 
    case '\t':
      printf("\\tab");
      return;
    case '\b':
      printf("\\backspace");
      return;
    default:
      printf("\\%c", value);
      return;
    }
  }

  if (is_integer(ctxt, v)) {
    printf("%d", as_integer(ctxt, v));
    return;
  }

  if (is_double(ctxt, v)) {
    printf("%.*f" , DBL_DIG, as_double(ctxt, v));
    return;
  }

  else {
    printf("<???:%llx>", v.as_uint64);
    return; 
  }
}

static void print_cons(context_p ctxt, value_t v) {
  print(ctxt, cons_car(ctxt, v));

  value_t tail = cons_cdr(ctxt, v);
  if (is_nil(ctxt, tail)) {
    printf(")");
    return;
  }
  if (is_cons(ctxt, tail)) {
    printf(" ");
    print_cons(ctxt, tail);
    return;
  }
  printf(" . ");
  print(ctxt, tail);
  printf(")");
}
