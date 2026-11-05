/* pthread driver for threads.fas: 4 threads x N increments, through an
   atomic add and through a cas spinlock, then a release/acquire handoff. */
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>

void fas_bump(int64_t *counter, int64_t n);
void fas_locked_bump(uint32_t *lock, int64_t *counter, int64_t n);
void fas_publish(int64_t *data, uint32_t *flag, int64_t value);
int64_t fas_consume(int64_t *data, uint32_t *flag);

enum { THREADS = 4, N = 1000000 };
static int64_t atomic_counter, locked_counter, data;
static uint32_t lock, flag;

static void *bump(void *arg) { (void)arg; fas_bump(&atomic_counter, N); return 0; }
static void *locked(void *arg) { (void)arg; fas_locked_bump(&lock, &locked_counter, N); return 0; }
static void *publish(void *arg) { (void)arg; fas_publish(&data, &flag, 4242); return 0; }

int main(void) {
  pthread_t t[THREADS];
  for (int i = 0; i < THREADS; i++) pthread_create(&t[i], 0, bump, 0);
  for (int i = 0; i < THREADS; i++) pthread_join(t[i], 0);
  for (int i = 0; i < THREADS; i++) pthread_create(&t[i], 0, locked, 0);
  for (int i = 0; i < THREADS; i++) pthread_join(t[i], 0);
  pthread_create(&t[0], 0, publish, 0);
  int64_t got = fas_consume(&data, &flag);
  pthread_join(t[0], 0);
  if (atomic_counter != (int64_t)THREADS * N || locked_counter != (int64_t)THREADS * N || got != 4242) {
    printf("atomic %lld locked %lld got %lld\n", (long long)atomic_counter,
           (long long)locked_counter, (long long)got);
    return 1;
  }
  return 0;
}
