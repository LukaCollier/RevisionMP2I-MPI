#ifndef heap
#define heap
#include <stdbool.h>

typedef struct heap_s heap_t;

heap_t *heap_init(int n);
void heap_free(heap_t *h);
bool heap_is_empty(heap_t *h);
void heap_add(heap_t *h, int vertex, int dist, int prio);
int extract_min(heap_t *h, int *vertex, int *dist);
#endif
