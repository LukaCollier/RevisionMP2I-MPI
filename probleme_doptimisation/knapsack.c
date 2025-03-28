#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <stdbool.h>
#include <assert.h>
#define N 1000
struct obj_s {
  double v;
  double w;
};
typedef struct obj_s obj_t ;

obj_t generate (){
  obj_t o;
  o.w = (double) rand()/RAND_MAX; // (double) force la conversion en double meme pour (int)
  double coins = (double) rand()/RAND_MAX;
  o.v= -log(coins);
  return o;
}

void  print_obj (obj_t o){
  printf("value : %lf , weight: %lf\n",o.v,o.w);
}
double *tabrap(obj_t ins[], int n){
  double *tabrap=calloc(n,sizeof(double));
  for(int i=0;i<n;i++){
    tabrap[i]=ins[i].v/ins[i].w;
  }
  return tabrap;
}

bool geq(obj_t o1,obj_t o2){
  return o1.v/o1.w >= o2.v/o2.w;
  //return o1.v >= o2.v;
}
void swap(obj_t tab[],int i, int j){
  obj_t tmp=tab[i];
  tab[i]=tab[j];
  tab[j]=tmp;
}
void sift_down(int n,int i,obj_t tab[N]){ //méthode definir rapidement un tri par tas avec déjà un tableau//
  while(i<n){
    int g=2*i +1;
    int d=g+1;
    int j=i;
    if(g <n && geq(tab[g],tab[j])){
      j=g;
    } //fonctionne
    if(d <n && geq(tab[d],tab[j])){ 
      j=d;
    }
    if (j==i){ 
      return;
    }
    swap(tab,i,j);
    i=j;
  }
}

void tri_tas(obj_t tab[N]){
  for(int i=N/2;i>=0;i--){
    sift_down(N,i,tab);
  }
  for (int i=N-1;i>=0;i--){
    swap(tab,0,i);
    sift_down(i,0,tab);
  }
}
double solveur_knapstar(obj_t tab[N],int n,double W_max){
  double W=0;
  double V=0;
  int i=n-1;
  while(i>=0){// n tour de boucle
    double wi=tab[i].w;
    if(wi+W>W_max){
      return V+tab[i].v*(W_max-wi)/wi;
    }
    W=W+wi;
    V=V+tab[i].v;
    i--;//i++
  }
  return V;
}

/*double solv_knap(obj_t tab[N],int n,double W_max){// sans branch and bound

  instance:
-avec:appel rec pour trouver mieux

-sans:appel réc uniquement si solve_star trouve mieux

  if(n==0){
    return 0.;
  }
  if(tab[n-1].w>W_max){
    return solv_knap(tab,n-1,W_max);
  }
  double avec=tab[n-1].v+solv_knap(tab,n-1,W_max-tab[n-1].w);
  double sans=solv_knap(tab,n-1,W_max);
  if (avec>sans){
    return avec;
  }
  return sans;
}
*/
double solve_knap(obj_t tab[N],int n,double W_max){
/*
  instance:
-avec:appel rec pour trouver mieux

-sans:appel réc uniquement si solve_star trouve mieux
*/
  if(n==0){
    return 0.;
  }
  if(tab[n-1].w>W_max){
    return solve_knap(tab,n-1,W_max);
  }
  double avec=tab[n-1].v+solve_knap(tab,n-1,(W_max-tab[n-1].w));
  if (solveur_knapstar(tab,n-1,W_max) < avec){
    return avec;
  }
  double sans =solve_knap(tab,n-1,W_max);
  if (avec>sans){
    return avec;
  }
  return sans;
}
int main(int argc,char *argv[]){
  srand(time(NULL));
  obj_t instance[N];
  for(int i=0;i<N;i++){
    instance[i]=generate();
    print_obj(instance[i]);
  }
  printf("\n");
  /*double *tab1=tabrap(instance,N);
  for(int i=0;i<N;i++){
    printf("%lf\n",tab1[i]);
  }*/
  tri_tas(instance);
  for(int i=0;i<N;i++){
    print_obj(instance[i]);
  }
  /*ouble *tab=tabrap(instance,N);
  for(int i=0;i<N;i++){
    printf("%lf\n",tab[i]);
  }*/
  double resknapstar=solveur_knapstar(instance, N,0.5);
  printf("%lf\n",resknapstar);
  double sol=solve_knap(instance,N,0.5);
  printf("valeur avec branch and bound %lf\n",sol);
  /*free(tab1);
  free(tab);*/
  return 0;
}
