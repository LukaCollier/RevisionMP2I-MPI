#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

typedef struct tree_s tree;

tree *leaf(bool b);//creates a new leaf. O(1)
tree *node(tree *l, int attr, tree *r);//creates a new node. O(1)
void free_tree(tree *t);//frees memory. O(|t|)
bool is_leaf(tree *t);//returns true iff t is a leaf. O(1)
int attribute(tree *t);//returns the label of a node. O(1)
bool accept(tree *t);//returns the label of a leaf. O(1)
bool decide(tree *t, bool* attributes);//apply the decision tree to a table of attributes

