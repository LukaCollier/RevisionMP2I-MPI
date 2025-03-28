#include "decisiontree.h"

struct tree_s{
  struct tree_s *left;
  struct tree_s *right;
  bool is_leaf;
  int attr;
  bool accept;
};
typedef struct tree_s tree;

tree *leaf(bool b){
  tree *t = malloc(sizeof(tree));
  t->left = NULL;
  t->right = NULL;
  t->is_leaf = true;
  t->accept = b;
  return t;
}

tree *node(tree *l, int attr, tree *r){
  tree *t = malloc(sizeof(tree));
  t->left = l;
  t->right = r;
  t->is_leaf = false;
  t->attr = attr;
  return t;
}

void free_tree(tree *t){
  if (t == NULL) { return; }
  free_tree(t->left);
  free_tree(t->right);
  free(t);
}

bool is_leaf(tree *t){
  return t->is_leaf;
}

int attribute(tree *t){
  assert(!is_leaf(t));
  return t->attr;
}

bool accept(tree *t){
  assert(is_leaf(t));
  return t->accept;
}

bool decide(tree *t, bool* attributes){
  return true;
}

