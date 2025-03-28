#include <stdio.h>
#include <math.h>
#include "decisiontree.h"

typedef bool *attributes;
struct example_s {
  attributes attr;
  char lbl;
};
typedef struct example_s example;

#define nb_ex 435
#define nb_attr 16

example db[nb_ex];

example read_example(FILE *f){
  example ex;
  fscanf(f,"%c,", &ex.lbl);
  ex.attr = malloc(nb_attr * sizeof(bool));
  for (int i = 0; i < nb_attr; i = i + 1){
    char c;
    fscanf(f,"%c,", &c);
    ex.attr[i] = true;
    if (c == 'n') { ex.attr[i] = false; }
  }
  fscanf(f,"\n");
  return ex;
}

double h(double num, double denom){//-p log p - (1-p) log (1- p), p = num/denom
  double p = num / denom;
  double q = 1 - p;
  if (p == 0 || p == 1) return 0;
  return - p * log2(p) - q * log2(q);
}

bool passe(int filtre[], example ex){
  for (int a = 0; a < nb_attr; a = a + 1)
    if ((ex.attr[a] && filtre[a] == -1)
        || (!ex.attr[a] && filtre[a] == 1))
      return false;
  return true;
}

void compte(int filtre[], int attr, int *cpt_vv, int *cpt_vf, int *cpt_fv, int *cpt_ff){
  *cpt_vv = 0; *cpt_vf = 0; *cpt_fv = 0; *cpt_ff = 0;
  for (int ex = 0; ex < nb_ex; ex = ex + 1){
    if (!passe(filtre, db[ex])) continue;
    example data = db[ex];
    if (data.attr[attr] && data.lbl == 'd') *cpt_vv = *cpt_vv + 1;
    else if (data.attr[attr])               *cpt_vf = *cpt_vf + 1;
    else if (data.lbl == 'd')               *cpt_fv = *cpt_fv + 1;
    else                                    *cpt_ff = *cpt_ff + 1;
  }
}

double uncertainty(int filtre[], int attr){
  int vv, vf, fv, ff;
  compte(filtre, attr, &vv, &vf, &fv, &ff);
  double vrais = vv + vf;
  double faux = fv + ff;
  double total = vrais + faux;
  double hv = h(vv, vrais);
  double hf = h(ff, faux);
  double h_new = (vrais * hv + faux * hf) / total;
  return h_new;
}

bool majorite(int filtre[]){
  int vv, vf, fv, ff;
  compte(filtre, 0, &vv, &vf, &fv, &ff);
  int vrais = vv + fv;
  int faux = vf + ff;
  return vrais > faux;
}

bool unanimite(int filtre[]){
  int vv, vf, fv, ff;
  compte(filtre, 0, &vv, &vf, &fv, &ff);
  int vrais = vv + fv;
  int faux = vf + ff;
  return vrais == 0 || faux == 0;
}

tree *id3(int filtre[]){
  // filtre[i] = +1/-1/0 si l'attr i est filtré true/false/non-filtré
  if (unanimite(filtre))
    return leaf(majorite(filtre));
  int best_attr = -1;
  double best_unc = 2;
  for (int a = 0; a < nb_attr; a = a + 1){
    if (filtre[a] != 0) continue;
    double unc_a = uncertainty(filtre, a);
    if (unc_a < best_unc){
      best_attr = a;
      best_unc = unc_a;
    }
  }
  if (best_attr == -1)
    return leaf(majorite(filtre));
  filtre[best_attr] = -1;
  tree *left = id3(filtre);
  filtre[best_attr] = 1;
  tree *right = id3(filtre);
  filtre[best_attr] = 0;
  return node(left, best_attr, right);
}

int main(int argc, char *argv[]){
  FILE *f = fopen("house-votes-84-bin.data","r");
  for (int i = 0; i < nb_ex; i = i + 1){
    db[i] = read_example(f);
  }
  fclose(f);

  /* TESTS */
  int filtre[nb_attr];
  for (int i = 0; i < nb_attr; i = i + 1)
    filtre[i] = 0;
  tree *t = id3(filtre);

   char *questions[] = {"handicapped-infants: 2 (y,n)", "water-project-cost-sharing: 2 (y,n)", "adoption-of-the-budget-resolution: 2 (y,n)", "physician-fee-freeze: 2 (y,n)", "el-salvador-aid: 2 (y,n)", "religious-groups-in-schools: 2 (y,n)", "anti-satellite-test-ban: 2 (y,n)", "aid-to-nicaraguan-contras: 2 (y,n)", "mx-missile: 2 (y,n)", "immigration: 2 (y,n)", "synfuels-corporation-cutback: 2 (y,n)", "education-spending: 2 (y,n)", "superfund-right-to-sue: 2 (y,n)", "crime: 2 (y,n)", "duty-free-exports: 2 (y,n)", "export-administration-act-south-africa: 2 (y,n)"};

   tree *curr = t;
   while (!is_leaf(curr)){
     printf("%s ?\n", questions[attribute(curr)]);
     char ans;
     scanf(" %c", &ans);
     if (ans == 'y') curr = curr->right;
     else curr = curr->left;
   }
   if (accept(curr))
     printf("You're a democrat !\n");
   else
     printf("You're a republican !\n");


  free_tree(t);
  for (int i = 0; i < nb_ex; i = i + 1){
    free(db[i].attr);
  }
  return 0;
}
