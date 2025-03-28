#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <time.h>
void mur(){
  printf("\u2588\u2588");
}

void rien(){
  printf("  ");
}
/*char *maketab(int n, int m){
  char *tab=malloc((n+2)*(m+2)*sizeof(char));
  for(int i=0;i<((n+2)*(m+2));i++){
    tab[i]='#';
  }
  return tab ;
}
*/
int find(int uf[], int i){
  int tmp = i;
  while (uf[tmp] >= 0) {
    tmp = uf[tmp];
  }
  if (i == tmp) {return i;}
  while (uf[i] != tmp) {
    int j = uf[i];
    uf[i] = tmp;
    i = j;
  }
  return tmp;
}

int unify(int uf[], int i, int j){
  i = find(uf, i);
  j = find(uf, j);
  if (uf[i] == uf[j]) {
    uf[i] = j;
    uf[j] = uf[j] - 1;
  }
  if (uf[i] < uf[j]) {
    uf[j] = i;
  }
  if (uf[i] > uf[j]) {
    uf[i] = j;
  }
}
char **maketab(int n, int m){
  char **tab=malloc((n+2)*sizeof(char *));
  for(int i=0;i< n+2 ;i++){
    tab[i]=malloc((m+2)*sizeof(char));
    for(int j=0;j<m+2;j++){
      tab[i][j]='#';
    }
  }
  return tab;
}
void printtab(char **tab,int n,int m){
  for(int i=0;i<(n+2);i++){
    for(int j=0;j<m+2;j++){
      if(tab[i][j]=='#'){
        mur();
      }
      else rien();
    }
    printf("\n");
  }
}
void freetab(char **tab,int n){
  for(int i=0;i<n+2;i++){
    free(tab[i]);
  }
  free(tab);
}/* ne fonctionne pas */
/*int main(int argc, char *argv[]){
  if( argc != 3){
    printf("deux arguments connard du virus");
    return 0;
  }
  int n = atoi(argv[1]);
  int m = atoi(argv[2]);
  srand(time(NULL));
  mur();
  rien();
  printf("\n");
  rien();
  mur();
  char **tab=maketab(n,m);
  uf_t uf = uf_init(n*m);
  for(int i=0;i<n*m-1;i++){
    int i =rand()%n;
    int j = rand()%m;
    tab[i+1][j+1]=' ';
  }
  printtab(tab,n,m);
  freetab(tab,n);
  uf_free(uf);
  return 0;
}*/
bool *init_mat(int n,int m){
  bool *mat=malloc(n*m*sizeof(bool));
  for(int i =0;i<n*m;i++){
    mat[i]=true;
  }
  return mat;
}
void pr(bool *mat,int i){
  if(mat[i]) mur();
  else rien();
}
int main(int argc, char *argv[]){
  // Lecture des arguments
  assert(argc > 2);
  int n = atoi(argv[1]);
  int m = atoi(argv[2]);

  // Initialisation
  int *uf = malloc(n*m*sizeof(int));
  bool *horizontal = init_mat(n,m);
  bool *vertical =init_mat(n,m);
  for (int i = 0; i < n*m; i = i + 1){
    uf[i] = -1;
  }
  srand(time(NULL));
  int cpt = 1;
  while (cpt < n*m){
    if (rand() % 2 == 0){
      int pos = (rand() % (n-1)) + n * (rand() % m);
      if (find(uf, pos) != find(uf, pos+1)) {
        unify(uf, pos, pos+1);
        horizontal[pos] = false;
        cpt = cpt + 1;
      }
    } else {
      int pos = (rand() % n) + n * (rand() % (m-1));
      if (find(uf, pos) != find(uf, pos+n)) {
        unify(uf, pos, pos+n);
        vertical[pos] = false;
        cpt = cpt + 1;
      }
    }
  }
  for (int i = 0; i < 2*n+1; i = i + 1){
    mur();
  }
  printf("\n");
  for (int j = 0; j < m; j = j + 1){
    mur();
    for (int i = 0; i < n; i = i + 1){
      rien(); pr(horizontal,j*n+i);
    }
    printf("\n"); mur();
    for (int i = 0; i < n; i = i + 1){
      pr(vertical,j*n+i); mur();
    }
    printf("\n");
  }

  // Cleanup
  free(uf);
  free(horizontal);
  free(vertical);

  return 0;
}
