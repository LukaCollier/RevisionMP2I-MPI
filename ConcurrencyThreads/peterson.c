#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>

#define N 2

pthread_t fils[N];
int cpt = 0;
/*sans peterson */
pthread_mutex_t verrou;
/*void *exec(void *args){
  int *id = args;
  for (int i = 0; i < 500000; i = i + 1){
    pthread_mutex_lock(&verrou);
    printf("%d", *id);
    cpt = cpt + 1;
    pthread_mutex_unlock(&verrou);
  }
  return NULL;
}*/
/*avec peterson */
bool veutentrer[N];
int tour;

void *exec(void *args){
  int *id = args;
  for (int i=0; i<10000;i++){
    int autre = 1-(*id);
    veutentrer[*id]=true;
    tour=autre;
    while (veutentrer[autre] && tour!=*id){}
    cpt=cpt+1;
    veutentrer[*id]=false;
  }
  return NULL;
}
int main(int argc, char *argv[]){
  int args[2];
  for (int i = 0; i < N; i = i + 1){
    args[i] = i;
    pthread_create(&fils[i], NULL, exec, &args[i]);
  }

  for (int i = 0; i < N; i = i + 1)
    pthread_join(fils[i], NULL);
  printf("\n%d\n", cpt);
  return 0;
}
