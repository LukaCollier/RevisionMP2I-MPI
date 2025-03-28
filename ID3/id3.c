#include <stdio.h>
#include "decisiontree.h"
#include <math.h>
#include <stdbool.h>
#define nb_ex 435
#define nb_attr 16 
typedef bool *attributes;
struct example_s {
  attributes attr;
  char lbl;
};
typedef struct example_s example;
example read_example(FILE *f){
  example ex;
  fscanf(f,"%c,", &ex.lbl);
  ex.attr = malloc(16 * sizeof(bool));
  for (int i = 0; i < 16; i = i + 1){
    char c;
    fscanf(f,"%c,", &c);
    ex.attr[i] = true;
    if (c == 'n') { ex.attr[i] = false; }
  }
  fscanf(f,"\n");
  return ex;
}
double entropie(double p){
  if( p <= 0 || p>= 1 ) return 0;
  return -p*(log2(p))-(1-p)*(log2(1-p));
}
int passe (int filtre[],example ex){
  for (int  a=0; a<nb_attr ; a++){
    if ((ex.attr[a] && filtre[a] == -1 ) || (!ex.attr[a] && filtre[a]== 1 )) return false;
  }
  return true;
}
void compte(example db[], int filtre[],int attr, int *cpt_vv,int *cpt_vf, int *cpt_fv,int *cpt_ff ){
  *cpt_vv=0;*cpt_vf=0;
  *cpt_fv=0;*cpt_ff=0;
  for(int i=0;i<nb_ex;i++){
    if (!passe(filtre,db[i])) continue;
    example data = db[i];
    if (data.attr[attr] && data.lbl =='d') *cpt_vv = *cpt_vv +1;
    else if(data.attr[attr])  *cpt_vf = *cpt_vf +1;
    else if (data.lbl=='d')  *cpt_fv = *cpt_fv +1;
    else  *cpt_ff = *cpt_ff +1;
  }
}
double avg(double p, double n){
  return -n*(entropie(p))-(1-n)*(entropie(1-p));
}
double ig(example db[],int filtre[], int attr){
  int vv,vf,fv,ff;
  compte(db,filtre,attr,&vv,&vf,&fv,&ff);
  double vrais =vv+vf;
  double faux = fv+ff;
  double total =vrais +faux;
  double hv =entropie((vv/vrais));
  double hf =entropie((ff/faux));
  double h_prev = entropie((vrais/total));
  double h_new =(vrais*hv +faux * hf)/total;
  return h_prev - h_new ;
}
/*
bool samebool(example db[],int n,int p,int r){
  if (p>r) return false;
  else{_i
    for(int i=0;i<n-1;i++){
      if (db[i].attr[p] != db[(i+1)].attr[p]) return false;
    }
    return true;
  }
}
int cptpos(example db[],int n,int p , int r){
  int cpt=0;
  if (p>r) return cpt;
  else{
    for(int i=0;i<n;i++){
      if (db[i].attr[p]) cpt ++;
    }
  }
  return cpt;
}
*/

bool majorite(int filtre [],example db[]){
  int vv,vf,fv,ff;
  compte(db,filtre,0,&vv,&vf,&fv,&ff);
  int vrais = vv +fv;
  int faux =ff+vf;
  return vrais>faux;
}
bool unanimite(int filtre[],example db[]){
  int vv,vf,fv,ff;
  compte(db,filtre,0,&vv,&vf,&fv,&ff);
  int vrais = vv +fv;
  int faux =ff+vf;
  return vrais==0 ||faux==0;
}
tree *id3( example db[], int filtre[]){
  // filtre[i]=+1/-1/0 si l'attr i est filtré true/false/non-filtré
  if(unanimite(filtre,db)) return leaf(majorite(filtre,db));
  int best_attr = -1;
  double best_ig = -1;
  for(int a=0 ;a< nb_attr;a++){
    if(filtre[a]!=0) continue;
    double iga = ig(db,filtre,a);
    if (iga > best_ig){
      best_attr = a;
      best_ig = iga;
    }
  }
  if (best_attr == -1){
    return leaf(majorite(filtre,db));
  }
  filtre [best_attr]=-1;
  tree *left = id3(db,filtre);
  filtre [best_attr]=1;
  tree *right = id3(db,filtre);
  filtre [best_attr]=0;
  return node(left,best_attr, right);
}

int main(int argc, char *argv[]){
  FILE *f = fopen("house-votes-84-bin.data","r");
  example db[nb_ex];
  for (int i = 0; i < nb_ex; i = i + 1){
    db[i] = read_example(f);
  }
  fclose(f);

  /* TESTS */
  int filtre[nb_attr];
  for(int i=0;i< nb_attr ; i++){
    filtre[i]=0;
  }
  tree *t = id3(db,filtre);
  free_tree(t);

  for (int i = 0; i < nb_ex; i = i + 1){
    free(db[i].attr);
  }
  return 0;
}
