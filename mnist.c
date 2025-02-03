#include <stdlib.h>
#include <stdio.h>
#include <math.h>
typedef unsigned char ** image;
int ntr = 60000, w=28, h=28,nte=10000;
void print_img(image img){
   for (int i = 0; i < h; i = i + 1) {
    for (int j = 0; j < w; j = j + 1) {
      printf("%4u",img[i][j]);
    }
    printf("\n");
   }
}
image *read_img_file(char filename[],int n){
  FILE *f = fopen(filename,"r");
  int magic[4];
  fread(magic,4,4,f);
  image *img = malloc(n*sizeof(image));
  for(int i =0; i<n;i++){
    img[i]=malloc(h*sizeof(unsigned char *));
    for(int k = 0;k<h;k++){
      img[i][k] =malloc(h*sizeof(unsigned char));
      for(int j=0;j<h;j++){
        fscanf(f,"%c",&img[i][k][j]);
      }
    }
  }
  fclose(f);
  return img;
}
char *read_lbl_file(char filename[],int n){
  FILE *f = fopen(filename,"r");
  int header[2];
  fread(header,4,2,f);
  char *lbl = malloc(n*sizeof(char));
  for(int i=0;i<n;i++){
    fscanf(f,"%c",&lbl[i]);
  }
  return lbl;
}
int dist(image i1,image i2){
  int res=0;
  for(int i=0;i<h;i++){
    for(int j=0;j<h;j++){
      int diff = i2[i][j]-i1[i][j];
      res = res +(diff*diff);
    }
  }
  res = sqrt(res);
  return res;
}
int imaxtab(int *tab,int k){
  int j =0;
  int max=tab[j];
  for(int i=1; i<k;i++){
    if(max<tab[i]){
      j=i;
      max=tab[i];
    }
  }
  return j;
}
int mrept(int *tab,int k){
  int tcpt[k];
  for(int i=0;i<k;i++){
    tcpt[i]=0;
  }
  for(int i=0;i<k;i++){
    tcpt[tab[i]]=tcpt[tab[i]]+1;
  }
  int max=0;
  int nmax=tcpt[0];
  for(int i=1;i<k;i++){
    if(nmax<tcpt[i]){
      max=i;
      nmax=tcpt[i];
    }
  }
  return max;
}
int classe(image *img,char *lbl,image im,int k,int n){
  int ck[k];
  int cd[k];
  int j=0,nbt=0;
  while(nbt < k){
      int d=dist(img[j],im);
      cd[nbt]=d;
      ck[nbt]='0'+lbl[j];
      nbt++;
      j++;
  }
  int imax=imaxtab(cd,k);
  while(j<n){
    int d=dist(img[j],im);
    if(cd[imax]>d){
      cd[imax]=d;
      ck[imax]=lbl[j];
      imax=imaxtab(cd,k);
    }
    j++;
  }
  int c=mrept(ck,k);
  return c;
}
int main(int argc, char *argv[]){
  /*printf("%x\n",*magic);*/
  char train[] = "train-images-idx3-ubyte";
  char test[] = "t10k-images-idx3-ubyte";
  image *img_train = read_img_file(train,ntr);
  image *img_test = read_img_file(test,nte);
  char *lbl_train = read_lbl_file("train-labels-idx1-ubyte",ntr);
  char *lbl_test = read_lbl_file("t10k-labels-idx1-ubyte",nte);
  for(int i=0;i<nte;i++){
    int c=classe(img_train,lbl_train,img_test[i],10,ntr);
    print_img(img_test[i]);
    printf("Classe : %c\n", '0' + lbl_test[i]);
    printf("%d\n",c);
  }
  /*print_img(img_train[1]);
  printf("Classe : %c\n", '0' + lbl_train[1]);
  int d = dist(img_train[0],img_train[1]);
  printf("%d\n",d);*/
  return 0;

}
