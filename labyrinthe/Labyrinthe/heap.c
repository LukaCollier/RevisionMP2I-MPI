#include "heap.h"
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
typedef struct item_s {
  int vertex;
  int dist;
  int prio;
} item;


struct heap_s{
  int max;
  int len;
  item *data;
};
typedef struct heap_s heap_t;
heap_t *heap_init(int n){
  heap_t *h=malloc(sizeof(heap_t));
  h->max = n;
  item *data=malloc(sizeof(item)*n);
  h->data=data;
  h->len = 0;
  return h;
}

void heap_free(heap_t *h){
  free(h->data);
  free(h);
}
void swap(heap_t *h,int i,int j){
  item tmp=h->data[i];
  h->data[i] = h->data[j];
  h->data[j]= tmp;
}
bool geq(item a,item b){
  if(a.prio>b.prio) return false;
  else return true;

}
void sift_down(heap_t *h,int i){
  while(i<h->len){
    int g=2*i+1;
    int d=2*i+2;
    int j=i;
    if((g<h->len) && geq(h->data[g],h->data[j])){
      j=g;
    }
    if((d<h->len) && geq(h->data[d],h->data[j])){
      j=d;
    }
    if(j==i){
      return;
    }
    swap(h,j,i);
    i=j;
  }
}

void tri_tas(heap_t *h){
  for(int i =0;i<(h->len/2);i++){
    sift_down(h,i);
  }
  for(int i=h->len -1; i>=0;i--){
    swap(h,i,0);
    sift_down(h,0);
  }
}
/*void heap_add2(heap_t *h, int vertex, int dist, int prio){
  item k={vertex,dist,prio};
  if(h->len>=h->max){
    return;
  }
  else{
    tri_tas(h);
  }
}*/
void heap_add(heap_t *h, int vertex, int dist, int prio){
  item k={vertex,dist,prio};
  if(h->len>=h->max){
    return;
  }
  else{
    int i=h->len;
    h->data[h->len]=k;
    h->len=h->len +1;
    while(i>0 && h->data[i].prio<h->data[(i-1)/2].prio){
      i=(i-1)/2;
      swap(h,i,(i-1)/2);
    }
  }
}

bool heap_is_empty(heap_t *h){
  return h->len ==0;
}
int extract_min(heap_t *h, int *vertex, int *dist){
  assert(h->len>0);
  item min = h->data[0];
  h->len=h->len -1;
  h->data[0]=h->data[h->len];
  sift_down(h,0);
  return min.prio;
}
