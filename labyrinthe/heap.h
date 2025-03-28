#ifndef heap
#define heap

typedef struct heap_s heap;

heap *heap_init(int n);
void heap_free(heap *h);
void *heap_add(heap *h,int vertex,int dist,int prio);
int extract_min(heap *h,int *vertex, int *dist);
#endif
