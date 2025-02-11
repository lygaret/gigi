#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "scheme.h"

#define BUFFER_MAX 2048

/* input stream */

static char peek(FILE *in);
static bool is_delimiter(char c);

static void consume_ws(FILE *in);
static void consume_line(FILE *in);

static bool try_consume_char(const char c, FILE *in);
static bool try_consume_chars(const char* match, int len, FILE *in);

/* reader */

static value_t read_integer(context_p ctxt, FILE *in);
static value_t read_double(context_p ctxt, uint32_t whole, FILE *in);
static value_t read_slashchar(context_p ctxt, FILE *in);
static value_t read_macrochar(context_p ctxt, FILE *in);
static value_t read_pair(context_p ctxt, FILE *in);
static value_t read_string(context_p ctxt, FILE *in);
static value_t read_symbol(context_p ctxt, FILE *in);
/* object *read_objvector(context_p ctxt, FILE *in); */

value_t read(context_p ctxt, FILE *in) {
  value_t v;

  consume_ws(in);
  char c = getc(in);

  if (c == EOF) {
    fprintf(stderr, "ok, bye\n");
    exit(1);
  }

  if (c == '(') {
    v = read_pair(ctxt, in);
  }
  else if (c == '\'') {
    v = read(ctxt, in);
    v = make_cons(ctxt, symquote, make_cons(ctxt, v, vnil));
  }
  else if (c == '#') {
    v = read_macrochar(ctxt, in);
  }
  else if (c == '\\') {
    v = read_slashchar(ctxt, in);
  }
  else if (c == '"') {
    v = read_string(ctxt, in);
  }
  else if (isdigit(c)) {
    ungetc(c, in);
    v = read_integer(ctxt, in);
  }
  else {
    ungetc(c, in);
    v = read_symbol(ctxt, in);
  }

  /* require a delimiter after input */
  if (is_delimiter(peek(in))) {
    return v;
  }
  else {
    return make_error(ctxt, __LINE__);
  }
}

/* '#' has already been read */
value_t read_macrochar(context_p ctxt, FILE *in) {
  char c;
  switch(c = getc(in)) {
  case 't':
    return vtrue;

  case 'f':
    return vfalse;

  /* case '[': */
  /*   return read_objvector(ctxt, in); */

  default:
    return make_error(ctxt, __LINE__);
  }
}

/* '\' has already been read */
value_t read_slashchar(context_p ctxt, FILE *in) {
  if (try_consume_chars("newline", 7, in)) {
    return make_character(ctxt, '\n');
  }
  else if (try_consume_chars("tab", 3, in)) {
    return make_character(ctxt, '\t');
  }
  else if (try_consume_chars("space", 5, in)) {
    return make_character(ctxt, ' ');
  }
  else if (try_consume_chars("backspace", 9, in)) {
    return make_character(ctxt, '\b');
  }
  else {
    char c = getc(in);
    return make_character(ctxt, c);
  }
}

/* '"' has already been read */
value_t read_string(context_p ctxt, FILE *in) {
  char c;
  char buffer[BUFFER_MAX];
  int len = 0;

  while((c = getc(in)) != '"') {
    if (c == EOF) {
      fprintf(stderr, "unterminated string literal\n");
      exit(1);
    }

    if (c == '\\') {
      c = getc(in);
      if (c == 'n') {
        c = '\n';
      } else if (c == 't') {
        c = '\t';
      }
    }

    if (len < BUFFER_MAX - 1) {
      buffer[len++] = c;
    }
    else {
      fprintf(stderr, "string too long, max length is %d", BUFFER_MAX);
      exit(1);
    }
  }
  
  buffer[len++] = '\0';
  return make_string(ctxt, buffer, len);
}

/* '#[' has already been consumed */
/* read as pairs, convert to vector once we know the length */
/* object *read_objvector_recur(context_p ctxt, FILE *in, int currlength) { */
/*   object *vector; */
/*   object *next_obj; */

/*   consume_ws(in); */
/*   if (try_consume_char(']', in)) { */
/*     /\* we hit the bottom, we know the length *\/ */
/*     return make_objvector(currlength, ctxt->nil); */
/*   } */

/*   next_obj = read(ctxt, in); */
/*   vector   = read_objvector_recur(ctxt, in, currlength + 1); */

/*   objvector_set(vector, currlength, next_obj); */
/*   return vector; */
/* } */

/* object *read_objvector(context_p ctxt, FILE *in) { */
/*   return read_objvector_recur(ctxt, in, 0); */
/* } */

value_t read_integer(context_p ctxt, FILE *in) {
  char c;
  long num = 0;

  /* consume binary strings */
  if (try_consume_chars("0b", 2, in)) {
    while((c = getc(in))) {
      if (c == '0') {
        num = (num * 2);
      }
      else if (c == '1') {
        num = (num * 2) + 1;
      }
      else {
        break;
      }
    }

    /* unread the non-digit */
    ungetc(c, in);
  }

  /* consume hex strings */
  else if (try_consume_chars("0x", 2, in)) {
    while((c = getc(in))) {
      if (isdigit(c)) {
        num = (num * 16) + (c - '0');
      }
      else if (c >= 'a' && c <= 'f') {
        num = (num * 16) + ((c - 'a') + 10);
      }
      else if (c >= 'A' && c <= 'F') {
        num = (num * 16) + ((c - 'A') + 10);
      }
      else {
        break;
      }
    }

    /* unread the non-digit */
    ungetc(c, in);
  }

  /* consume decimal strings */
  else {
    while(isdigit(c = getc(in))) {
      num = (num * 10) + (c - '0');
    }

    if (c == '.') {
      return read_double(ctxt, num, in);
    }

    /* unread the non-digit */
    ungetc(c, in);
  }

  return make_integer(ctxt, (uint32_t)num);
}

/* whole number part and decimal point have already been read */
static value_t read_double(context_p ctxt, uint32_t whole, FILE *in) {
  double num = whole; // implicit conversion

  char   c;
  double i = 1.0;

  while(isdigit(c = getc(in))) {
    i = i / 10.0;
    num = num + (i * (c - '0'));
  }

  /* unread the last non-digit */
  ungetc(c, in);

  return make_double(ctxt, num);
}

value_t read_symbol(context_p ctxt, FILE *in) {
  char c;
  char buffer[BUFFER_MAX];
  int len = 0;

  while (1) {
    c = getc(in);
    if (is_delimiter(c) || isspace(c))
      break;

    if (len < BUFFER_MAX - 1) {
      buffer[len++] = c;
    }
    else {
      fprintf(stderr, "string too long, max length is %d", BUFFER_MAX);
      exit(1);
    }
  }

  ungetc(c, in);
  return make_symbol(ctxt, buffer, len);
}

/* the opening paren has already been read */
value_t read_pair(context_p ctxt, FILE *in) {
  value_t car_obj;
  value_t cdr_obj;

  consume_ws(in);
  if (try_consume_char(')', in)) {
    /* closing paren means (), means nil */
    return vnil;
  }

  car_obj = read(ctxt, in);

  consume_ws(in);
  if (try_consume_char('.', in)) {
    /* improper list means explicit cdr, and explicit close paren */
    cdr_obj = read(ctxt, in);

    consume_ws(in);
    if (!try_consume_char(')', in)) {
      fprintf(stderr, "expecting ')' to close an improper list");
      exit(1);
    }
  }
  else {
    /* proper list means implicit cdr, through recursion */
    cdr_obj = read_pair(ctxt, in);
  }

  return make_cons(ctxt, car_obj, cdr_obj);
}

/* lexing helpers */

char peek(FILE *in) {
  char c = getc(in);
  ungetc(c, in);

  return c;
}

bool is_delimiter(char c) {
  return
    isspace(c) || c == EOF ||
    c == '(' || c == ')' ||
    c == '[' || c == ']' ||
    c == '"' || c == ';' ;
}

void consume_ws(FILE *in) {
  char c;
  while ((c = getc(in)) != EOF) {
    if (isspace(c)) {/* skip whitespace */
      continue;
    }
    else if (c == ';') {/* skip comments (to the end of the line) */
      consume_line(in);
      continue;
    }
    else {/* otherwise, we didn't get whitespace, bounce */
      ungetc(c, in);
      break;
    }
  }
}

void consume_line(FILE *in) {
  char c;
  while ((c = getc(in)) != EOF) {
    if (c == '\n') break;
  }
}

bool try_consume_char(char c, FILE *in) {
  char inc = getc(in);
  if (c != inc) {
    ungetc(inc, in);
  }

  return c == inc;
}

bool try_consume_chars(const char* match, int len, FILE *in) {
  const char *cursor = match;
  for (int i = 0; i < len; i++, cursor++) {
    char c = getc(in);

    /* put the _last_ character read back on,
       and then undo any match characters we've already read */
    if (c != *cursor) {
      ungetc(c, in);
      for (cursor--; cursor >= match; cursor--) {
        ungetc(*cursor, in);
      }

      return false;
    }
  }

  return true;
}
