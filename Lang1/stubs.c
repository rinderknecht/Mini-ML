#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/alloc.h>
#include <limits.h>
#include <stdlib.h>

CAMLprim value OS_realpath (value a_path)
{
  char* path = String_val (a_path);
  char* res = realpath (path, NULL);
  if (res) {
    value r_path = caml_copy_string (res);
    free (res);
    return r_path;
  }
  return a_path;
}
