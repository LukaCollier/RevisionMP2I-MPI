/*Boucle infini à corriger */


#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#define nb_grille 19683
typedef int grille;
struct pile_s{
  int n;
  int hd;
  struct pile_s *q; };

typedef struct pile_s pile;
bool gagnant[nb_grille];
int **makematrice(int n){
  int **mat=malloc(n*sizeof(int));
  for(int i=0;i<n;i++){
    mat[i]=malloc(n*sizeof(int));
  }
  return mat;
}
int traiteluka(grille g){//Dit de qui c'est le tour
  assert(g<nb_grille);
  int cr=0;
  int ce=0;
  int nu=0;
  while(g>0){
    int k = g % 3;
    if(k==0) nu++;
    if(k==1) ce++;
    else cr++;
    g=g/3;
  }
  int cpt=ce-cr;
  if (cpt<0 || cpt>1) return -1;
  return cpt + 1;
}

int traiteprof(grille g){
  assert(0<g<nb_grille);
  int cpt[3]={0,0,0};
  while(g>0){
    cpt[g%3] = cpt[g%3]+1;
    g=g/3;
  }
  int t=cpt[1]-cpt[2];
  if(t<0 || t>1) return -1;
  return t + 1;
}

int degree(grille g){
  int cpt= 0;
  for(int i=0;i<9;i++){
    if(g%3==0) cpt++;
    g=g/3;
  }
  return cpt;
}
bool in_g1(grille g){
  int tab[3][3];
  for(int i=0;i<3;i++){
    for(int j=0;j<3;j++){
      tab[i][j]= g%3;
      g=g/3;
      }
    }
  for(int i=0;i<3;i++){
    if(tab[0][i]==1||tab[1][i]==1 ||tab[2][i]==1) return true;
    if(tab[i][0]==1||tab[i][1]==1 ||tab[i][2]==1) return true;
  }
  return (tab[0][0]==1||tab[1][1]==1 ||tab[2][2]==1) || (tab[2][0]==1||tab[1][1]==1 ||tab[0][2]==1);
}

void parcours(){
  int pile[9*nb_grille];
  int sp=0; //stack pointer
  int deg[nb_grille];
  for(int g=0; g<nb_grille;g++){
    if(traiteprof(g)!= -1) deg[g] =degree(g);
    if(in_g1(g)){
      gagnant[g]=true;
      pile[sp]=g;
      sp++;
    }
  }
  while(sp >0){
    sp--;
    int g=pile[sp];
    if(traiteprof(g)==1){
      int g1= g; int delta=1;
      while(g1>0){
        if(g1%3==2){
          int voisin = g1 - 2* delta;
          deg[voisin]--;
          if(deg[voisin]==0){
            gagnant[voisin]=true;
            pile[sp]=voisin;
            sp++;
          }
        }
        delta=delta*3;
        g1/3;
      }
    }
    if(traiteprof(g)==1){
      int g1= g; int delta=1;
      while(g1>0){
        if(g1%3==1){
          int voisin = g1 - 1* delta;
          if(!gagnant[voisin]){
            gagnant[voisin]=true;
            pile[sp]=voisin;
            sp++;
          }
        }
        delta=delta*3;
        g1/3;
      }
    }
  }
}

void print_symbole(int g){
  if(g%3==0) printf("| |");
  if(g%3==1 ) printf("|O|");
  if(g%3==2 ) printf("|X|");
}
int main(){
  parcours();
  for(int g=0;g<nb_grille;g++){
    if(gagnant[g]){
      for(int i =0;i<3;i++){
        for(int j=0;j<3;j++){
          print_symbole(g);
        }
        printf("\n");
      }
    }
  }
  return 0;
}
