#include "heap.h"

typedef struct heap_s heap_t;
typedef struct item_s {
  int vertex;
  int dist;
  int prio
} item;


struct heap_s{
  int max;
  int len;
  item *data
};

heap_t *heap_init(int n){
  heap_t h=malloc(sizeof(heap_t));
  h->max = n;
  data=malloc(sizeof(item)*n);
  h->len = 0:
  return h;
}

void heap_free(heap_t *h){
  free(h->data);
  free(h);
}

void heap_add(heap_t *h, int vertex, int dist, int prio){
  item k={vertex,dist,prio}
}
/*terminé dans le dossier labyrinte*/
int extract_min(heap_t *h, int *vertex, int *dist){
}
