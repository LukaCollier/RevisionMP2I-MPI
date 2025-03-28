#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>
#include <unistd.h>
#define N 3


pthread_t fils[N];
int couvert[N]; // -1 si personne i n'a de couvert j sinon pour la personne j
void *exec(void *args){
  int *id= args;
  int i=*id;
  while(true){
    while(couvert[i]!= -1) {}
    couvert[i]=*id;
    while(couvert[(i+1)%N] != -1) {}
    couvert[(i+1)%N]=i;
    printf("%d mange\n",i);
    sleep(1);
    couvert[i]=-1;
    couvert[(i+1)%N]=-1;
  }
  return NULL;
}
int main(int argc, char *argv[]){
  int args[N];
  for(int i =0; i<N;i++){
    couvert[i] =-1;
  }
  for(int i = 0; i<N;i++){
    args[i]=i;
    pthread_create(&fils[i],NULL,exec,&args[i]);
    /*pthread_join(fils[i],NULL);*/
  }
  for(int i = 0; i<N;i++){
    pthread_join(fils[i],NULL);
  }
  return 0;

}
