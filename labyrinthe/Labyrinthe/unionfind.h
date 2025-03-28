#ifndef unionfind
#define unionfind
#include <stdbool.h>

typedef struct uf_s uf_t;

uf_t *uf_init(int size);
void uf_free(uf_t *uf);
int uf_find(uf_t *uf, int i);
bool uf_union(uf_t *uf, int i, int j);
#endif
