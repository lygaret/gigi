#include <stdlib.h>
#include <stdio.h>
#include "scheme.h"

int main (void) {
  printf("bootstrap scheme...\nuse ctrl-d to exit.\n");

  value_t v;
  context_p ctxt = alloc_context(4096);
  ctxt->curr_env = enhance_native_environment(ctxt);

  while (1) {
    printf("> ");
    v = read(ctxt, stdin);
    v = eval(ctxt, v, &ctxt->curr_env);

    if (is_error(ctxt, v)) {
      printf("!!! error: %llx", v.as_uint64);
    }
    else {
      print(ctxt, v);
    }

    printf("\n");
  }

  return 0;
}
