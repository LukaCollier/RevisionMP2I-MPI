#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <stdbool.h>



pthread_t fils[5];
int cpt =0;
void *exec(void *args){
  int *id = args;
  for(int i =0;i<500000;i++){
    cpt=cpt+1;
  }
  return NULL;
}
int main(int argc, char *argv[]){
  int id1=1;
  int id2=2;
  for(int i = 0; i<5;i++){
    pthread_create(&fils[i],NULL,exec,&id1);
    /*pthread_join(fils[i],NULL);*/
  }
  for(int i = 0; i<5;i++){
    pthread_join(fils[i],NULL);
  }
  printf("%d\n", cpt);
  return 0;

}
