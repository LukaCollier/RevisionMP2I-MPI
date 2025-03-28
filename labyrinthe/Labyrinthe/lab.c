#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <stdbool.h>
#include "unionfind.h"
#include "heap.h"


char **new_matrix(int n, int m){
  char **mat = malloc(n * sizeof(char *));
  for (int i = 0; i < n; i = i + 1){
    mat[i] = malloc(m * sizeof(bool));
    for(int j = 0; j < m; j = j + 1)
      mat[i][j] = "#";
  }
  return mat;
}

void free_matrix(bool **mat, int n){
  for (int i = 0; i < n; i = i + 1)
    free(mat[i]);
  free(mat);
}

void mur(int n){
  for (int i = 0; i < n; i = i + 1)
    printf("\u2588\u2588");
}

void rien(){
  printf("  ");
}

void plot(char f){
  if (f=="#") rien();
  if (f=="2") mur;
  else {
    printf("1 ");
  }
}

void nl(){
  printf("\n");
}

int main(int argc, char *argv[]){
  if (argc < 3){
    printf("Veuillez entrer au moins 2 arguments.\n");
    return 1;
  }
  int n = atoi(argv[1]);
  int m = atoi(argv[1]);
  srand(time(NULL));

  // Structures de données
  char **horizontal = new_matrix(n, m-1);
  char **vertical   = new_matrix(n-1, m);
  uf_t *uf = uf_init(n * m);

  // Génération du labyrinthe
  for (int cpt = 0; cpt < n * m - 1;){
    if (rand() % 2 == 0){// Horizontal
      int i = rand() % n;
      int j = rand() % (m - 1);
      int s = i * m + j;
      int t = s + 1;
      if (uf_union(uf, s, t)){
        cpt = cpt + 1;
        horizontal[i][j] = "2";
      }
    } else {// Vertical
      int i = rand() % (n-1);
      int j = rand() % m;
      int s = i * m + j;
      int t = s + n;
      if (uf_union(uf, s, t)){
        cpt = cpt + 1;
        vertical[i][j] = "2";
      }
    }
  }
  int s= rand()%(n*m);
  int t= rand()%(n*m);
  heap_t *h=heap_init(n*m);
  int *dist=malloc(n*m*sizeof(int));
  /*dijkstra */
  heap_add(h,s,0,0);
  while(!heap_is_empty(h)){
    int u,d;
    extract_min(h,&u,&d);
    if(u==t) break;
    if( d>=dist[u]) continue;
    dist[u]=d;
    int i= u/m;
    int j= u%m;
    horizontal[i][j] = "1";
    vertical[i][j] = "1";
    if(i>0 && vertical[i-1][j]){
      heap_add(h,u-m,d+1,d+1);
    }
    if(j>0 && horizontal[i][j-1]){
      heap_add(h,u-1,d+1,d+1);
    }
    if(i<n && vertical[i][j]){
      heap_add(h,u+1,d+1,d+1);
    }
    if(j<m-1 &&  horizontal[i][j]){
      heap_add(h,u+1,d+1,d+1);
    }
  }
  // Affichage
  mur(2*m+1); nl();
  for (int i = 0; i < n-1; i = i + 1){
    mur(1);
    for (int j = 0; j < m-1; j = j + 1){
      rien(); plot(horizontal[i][j]);
    }
    rien(); mur(1);
    nl();
    mur(1);
    for (int j = 0; j < m; j = j + 1){
      plot(vertical[i][j]); mur(1);
    }
    nl();
  }
  mur(1);
  for (int j = 0; j < m-1; j = j + 1){
    rien(); plot(horizontal[n-1][j]);
  }
  rien(); mur(1);
  nl();
  mur(2*m+1); nl();

  // Libération de la mémoire
  uf_free(uf);
  free_matrix(horizontal, n);
  free_matrix(vertical, n-1);
  heap_free(h);
  return 0;
}
