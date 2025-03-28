#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <pthread.h>
#include <assert.h>
#define N 2

typedef struct file_s {
  int *tab;
  int ihd;
  int len;
  int max;
  sem_t empty;
  sem_t filled;
  pthread_mutex_t m;
} file;

file *init(int max){
  file *f=malloc(sizeof(file));
  int *tab= malloc(max*sizeof(int));
  f->max=max;
  f->len=0;
  f->ihd;
  sem_init(&f->filled,0,0);
  sem_init(&f->empty,0,max);
  return f;
}

void push( file *f,int i){
  assert(f->len < f->max);
  sem_wait(&f->empty);
  pthread_mutex_lock(&f->m);
  f->tab[((f->ihd + f->len)+1) % f->max] =i;
  f->len = f->len +1 ;
  pthread_mutex_unlock(&f->m);
  sem_post(&f->empty);
}

int pop(file *f){
  assert(f->len > 0);
  sem_wait(&f->filled);
  pthread_mutex_lock(&f->m);
  int res= f->tab[f->ihd];
  f->ihd = (f->ihd +1)% f->max;
  f->len = f->len -1;
  pthread_mutex_unlock(&f->m);
  sem_post(&f->filled);
  return res;
}
void freef(file *f){
  if( f==NULL){
    return;
  }
  free(f->tab);
  free(f);
}

file *queue[N];
int cpt=0;
pthread_mutex_t m;
void add_print(int n){
  pthread_mutex_lock(&m);
  cpt=cpt+n;
  printf("%d\n",cpt);
  pthread_mutex_unlock(&m);
}
void m_print(int n){
  pthread_mutex_lock(&m);
  cpt=cpt-n;
  printf("%d\n",cpt);
  pthread_mutex_unlock(&m);
}
int rng (void){
  return rand() %100;
}

void *producteur(void * args){
  while(true){
    int x=rng();
    push(queue[x%2],x);
  }
  return NULL;
}
void *consommateur(void *args){
  int *id=args;
}

int main (int argc,char *argv[]){
  srand(time(NULL));
  for(int i =0; i<N;i++){
    queue[i]=init(256);
  }
  pthread_t threads[2*N];
  int args[N];
  for(int i=0;i<N;i++){
    pthread_create(&threads[i],NULL,consommateur,&args[i]);
    pthread_create(&threads[i+N],NULL,consommateur,&args[i]);
  }
  for(int i=0;i<2*N;i++){
  pthread_join(threads[i],NULL);
  }
  for(int i=0;i<2*N;i++){
    freef(queue[i]);
  }
  free(queue);
  return 0;
}
