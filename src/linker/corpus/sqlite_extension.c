#include "sqlite3ext.h"
SQLITE_EXTENSION_INIT1

static void answer(sqlite3_context *ctx, int argc, sqlite3_value **argv) {
  (void)argc;
  (void)argv;
  sqlite3_result_int(ctx, 42);
}

int sqlite3_fasprobe_init(sqlite3 *db, char **error, const sqlite3_api_routines *api) {
  SQLITE_EXTENSION_INIT2(api);
  (void)error;
  return sqlite3_create_function(db, "fas_probe", 0, SQLITE_UTF8, 0, answer, 0, 0);
}
