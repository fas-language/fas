/* Real SQLite API qualification. Every run gets a fresh database directory. */
#include "sqlite3.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int destroyed;

static void fail(sqlite3 *db, const char *what, int rc) {
  fprintf(stderr, "%s: %d: %s\n", what, rc, db ? sqlite3_errmsg(db) : "no database");
  exit(1);
}

static void sql(sqlite3 *db, const char *text) {
  int rc = sqlite3_exec(db, text, 0, 0, 0);
  if (rc != SQLITE_OK) fail(db, text, rc);
}

static void number(sqlite3 *db, const char *text, sqlite3_int64 expected) {
  sqlite3_stmt *stmt = 0;
  int rc = sqlite3_prepare_v2(db, text, -1, &stmt, 0);
  if (rc != SQLITE_OK) fail(db, text, rc);
  if (sqlite3_step(stmt) != SQLITE_ROW || sqlite3_column_int64(stmt, 0) != expected)
    fail(db, text, -1);
  if (sqlite3_step(stmt) != SQLITE_DONE) fail(db, text, -2);
  if (sqlite3_finalize(stmt) != SQLITE_OK) fail(db, text, -3);
}

static void plus_one(sqlite3_context *ctx, int argc, sqlite3_value **argv) {
  if (argc != 1 || sqlite3_user_data(ctx) != &destroyed) exit(1);
  sqlite3_result_int64(ctx, sqlite3_value_int64(argv[0]) + 1);
}

static void destroy(void *value) { ++*(int *)value; }

struct worker { const char *path; int first; };

static void *insert_rows(void *arg) {
  struct worker *worker = arg;
  sqlite3 *db = 0;
  sqlite3_stmt *stmt = 0;
  if (sqlite3_open(worker->path, &db) != SQLITE_OK) fail(db, "thread open", -1);
  sqlite3_busy_timeout(db, 10000);
  sql(db, "BEGIN IMMEDIATE");
  if (sqlite3_prepare_v2(db, "INSERT INTO items VALUES(?1,'payload')", -1, &stmt, 0))
    fail(db, "thread prepare", -1);
  for (int i = worker->first; i < worker->first + 50; ++i) {
    if (sqlite3_bind_int(stmt, 1, i) || sqlite3_step(stmt) != SQLITE_DONE ||
        sqlite3_reset(stmt)) fail(db, "thread insert", -1);
  }
  if (sqlite3_finalize(stmt)) fail(db, "thread finalize", -1);
  sql(db, "COMMIT");
  if (sqlite3_close(db)) fail(db, "thread close", -1);
  return 0;
}

int main(int argc, char **argv) {
  sqlite3 *db = 0, *backup = 0;
  sqlite3_stmt *stmt = 0;
  if (argc != 3) return 2;
  if (sqlite3_open(argv[1], &db)) fail(db, "open", -1);
  sqlite3_busy_timeout(db, 10000);
  sql(db, "PRAGMA journal_mode=WAL; CREATE TABLE items(id INTEGER PRIMARY KEY, value TEXT)");
  number(db, "SELECT count(*) FROM pragma_journal_mode WHERE journal_mode='wal'", 1);
  sql(db, "BEGIN IMMEDIATE");
  if (sqlite3_prepare_v2(db, "INSERT INTO items VALUES(?1,?2)", -1, &stmt, 0))
    fail(db, "prepare", -1);
  for (int i = 0; i < 200; ++i) {
    if (sqlite3_bind_int(stmt, 1, i) ||
        sqlite3_bind_text(stmt, 2, "payload", -1, SQLITE_TRANSIENT) ||
        sqlite3_step(stmt) != SQLITE_DONE || sqlite3_reset(stmt))
      fail(db, "insert", -1);
  }
  if (sqlite3_finalize(stmt)) fail(db, "finalize", -1);
  sql(db, "COMMIT; BEGIN; DELETE FROM items; ROLLBACK");
  sql(db, "SAVEPOINT inner; DELETE FROM items; ROLLBACK TO inner; RELEASE inner");
  number(db, "SELECT count(*) FROM items WHERE value='payload'", 200);
  number(db, "SELECT json_extract('{\"answer\":42}', '$.answer')", 42);
  sql(db, "CREATE VIRTUAL TABLE words USING fts5(value);"
          "INSERT INTO words VALUES('the quick brown fox'),('slow turtle')");
  number(db, "SELECT count(*) FROM words WHERE words MATCH 'quick'", 1);
  sql(db, "CREATE VIRTUAL TABLE boxes USING rtree(id,x0,x1,y0,y1);"
          "INSERT INTO boxes VALUES(1,0,10,0,10),(2,20,30,20,30)");
  number(db, "SELECT count(*) FROM boxes WHERE x0<=5 AND x1>=5", 1);
  if (sqlite3_create_function_v2(db, "plus_one", 1, SQLITE_UTF8, &destroyed,
                               plus_one, 0, 0, destroy)) fail(db, "callback", -1);
  number(db, "SELECT plus_one(41)", 42);
  if (strcmp(argv[2], "-")) {
    char *error = 0;
    sqlite3_enable_load_extension(db, 1);
    int rc = sqlite3_load_extension(db, argv[2], "sqlite3_fasprobe_init", &error);
    if (rc != SQLITE_OK) {
      fprintf(stderr, "extension: %s\n", error ? error : "unknown");
      sqlite3_free(error);
      fail(db, "load extension", rc);
    }
    number(db, "SELECT fas_probe()", 42);
  }
  pthread_t threads[4];
  struct worker workers[4];
  for (int i = 0; i < 4; ++i) {
    workers[i] = (struct worker){argv[1], 200 + i * 50};
    if (pthread_create(&threads[i], 0, insert_rows, &workers[i])) return 1;
  }
  for (int i = 0; i < 4; ++i) if (pthread_join(threads[i], 0)) return 1;
  number(db, "SELECT count(*) FROM items", 400);
  number(db, "SELECT sum(id) FROM items", 79800);
  if (sqlite3_open(":memory:", &backup)) fail(backup, "backup open", -1);
  sqlite3_backup *copy = sqlite3_backup_init(backup, "main", db, "main");
  if (!copy || sqlite3_backup_step(copy, -1) != SQLITE_DONE || sqlite3_backup_finish(copy))
    fail(backup, "backup", -1);
  number(backup, "SELECT sum(id) FROM items", 79800);
  number(db, "SELECT count(*) FROM pragma_integrity_check WHERE integrity_check!='ok'", 0);
  if (sqlite3_close(backup) || sqlite3_close(db)) return 1;
  if (destroyed != 1) return 1;
  if (sqlite3_open(argv[1], &db)) fail(db, "reopen", -1);
  number(db, "SELECT sum(id) FROM items", 79800);
  if (sqlite3_close(db)) return 1;
  puts("sqlite-api: rows=400 sum=79800 json=42 fts=1 rtree=1 backup=ok callback=ok reopen=ok");
  return 0;
}
