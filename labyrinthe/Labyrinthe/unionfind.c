#include "unionfind.h"
#include <stdlib.h>
#include <stdbool.h>

struct uf_s{
  int *parent;
  int *rang;
};
typedef struct uf_s uf_t;

uf_t *uf_init(int size){
  uf_t *uf = malloc(sizeof(uf_t));
  uf->parent = malloc(size * sizeof(int));
  uf->rang = malloc(size * sizeof(int));
  for (int i = 0; i < size; i = i + 1){
    uf->parent[i] = i;
    uf->rang[i] = 0;
  }
  return uf;
}

void uf_free(uf_t *uf){
  free(uf->parent);
  free(uf->rang);
}

int uf_find(uf_t *uf, int i){
  while (uf->parent[i] != i)
    i = uf->parent[i];
  return i;
}

bool uf_union(uf_t *uf, int i, int j){
  i = uf_find(uf, i);
  j = uf_find(uf, j);
  if (i == j) return false;
  if (uf->rang[i] < uf->rang[j]) uf->parent[i] = j;
  if (uf->rang[j] < uf->rang[i]) uf->parent[j] = i;
  if (uf->rang[i] == uf->rang[j]){
    uf->parent[i] = j;
    uf->rang[j] = uf->rang[j] + 1;
  }
  return true;
}
