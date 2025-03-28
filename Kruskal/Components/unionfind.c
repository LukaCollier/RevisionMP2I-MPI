#include <stdlib.h>
#include <stdio.h>

int *uf_create(int n){
  int *tab=malloc(n*sizeof(int));
  for(int i=0;i<n;i++){
    tab[i]=-1;
    }
  return tab;
}
void freeuf(int *uf){
  if(uf==NULL){
    return;
  }
  free(uf);
int find(int* uf, int i){
  int j=i;
  while(uf[j]>-1){
    j=uf[j];
  }
  return j;
}

void uf_union(int *uf , int i , int j){



}

int main(){
  return 0:
}
