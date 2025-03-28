#include "unionfind.h"

struct uf_s {
  int *parent;
  int *rang;
};
typedef struct uf_s uf_t;

uf_t uf_init(int size){
  uf_t uf;
  uf.parent=malloc(size * sizeof(int));
  uf.rang=malloc(size * sizeof(int));
  for(int i =0; i<size;i++){
    uf.parent[i]=i;
    uf.rang[i] =0;
  }
  return uf;
}

int uf_find(uf_t uf, int i){
  if (uf.parent[i] == i){
    return i;
  }
  uf.parent[i]= uf_find(uf,uf.parent[i]);
  return uf.parent[i];
}

void uf_union(uf_t uf ,int i, int j){
  int i1=uf_find(uf,i);
  int j1=uf_find(uf,j);
  if (i1 != j1) {
    if (uf.rang[i1] < uf.rang[j1]) {
      uf.parent[j1] = i1;
    } else if (uf.rang[i1] > uf.rang[j1]) {
      uf.parent[i1] = j1;
    } else {
      uf.parent[j1] = i;
      uf.rang[i1]=uf.rang[i1]+1;
    }
  }
}
/*
void uf_free(uf_t uf){
  free(uf.parent);
  free(uf.rang);
}*/
