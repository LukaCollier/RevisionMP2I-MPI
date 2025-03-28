#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

struct maillon_t {
  int val;
  struct maillon_t *next;
};
typedef struct maillon_t maillon;
struct graph_t{
  int nx;
  int ny;
  int **X;
  int **Y;
};
typedef struct graph_t graph;
struct couplage_t{
  int *mx;
  int *my;
};
typedef struct couplage_t couplage;
maillon *cons(int x,maillon *t){
  maillon *m = malloc(sizeof(maillon));
  m->val = x;
  m->next=t;
  return m;
}

int head(maillon *t){
  return t->val;
}
maillon *tail(maillon *t){
  return t->next;
}
void freemaillon(maillon *t){
  if(t->next == NULL) return ;
  else {
    freemaillon(t->next);
    free(t);
  }
}
void affiche(maillon *t){
  while(t !=NULL){
    printf("%d -> ",head(t));
    t=tail(t);

  }
  printf("\n");
}
maillon *chemin_augmentant(graph g, couplage m){
  bool *vuX=malloc(g.nx * sizeof(bool));
  //bool *vuY=malloc(g.ny *sizeof(bool));
  for (int i=0;i<g.nx;i++) vuX[i]=false;
  //for (int i=0;i<g.ny;i++) vuY[i]=false;
  for(int r=0; r<g.nx;r++){
    if (m.mx[r]== -1){// i sommet libre
      /*Parcours du graphe à partir du sommet i*/ 
      maillon *stack=NULL;
      maillon *chemin=NULL;
      stack=cons(r,stack);
      chemin=cons(r,chemin);
      while(stack!=NULL){
        int i =head(stack);
        if(vuX[i]){
          stack=tail(stack);
          chemin=tail(chemin);
        }
        else {
          vuX[i]=true;
          if(r!=i) {
            chemin=cons(m.mx[i],chemin);
          }
          chemin=cons(i,chemin);
          for(int k=0 ;g.X[i][k]!= -1 ;k++){
            int j=g.X[i][k];
            if (m.my[j] == -1){
              freemaillon(stack);
              return cons(j,chemin);}
            if (!vuX[m.my[j]])stack= cons(m.my[j],stack);
          }
        }
      }
    }
  }
  free(vuX);
  return NULL;
}

int main(int argc,char *argv[]){
  int x0[2]={1,-1};
  int x1[3]={0,2,-1};
  int x2[3]={0,1,-1};
  int x3[3]={0,1,-1};
  int x[4]={x0,x1,x2,x3};
  int y0[4]={1,2,3,-1};
  int y1[4]={0,2,3,-1};
  int y2[2]={1,-1};
  int y[3]={y0,y1,y2};
  graph g={4,3,x,y};
  int mx[4]={-1,0,1,-1};
  int my[3]={1,2,-1};
  couplage m={mx,my};
  maillon *chemin=chemin_augmentant(g,m);
  affiche(chemin);
  return 0;
}
