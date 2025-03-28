#ifndef unionfind
#define unionfind
struct uf_s{
  int *parent;
  int *rang;
};
typedef struct uf_s uf_t;

uf_t uf_init(int size);
int uf_find(uf_t uf, int i);
void uf_union (uf_t uf,int i,int j);
/*void uf_free(uf_t uf);*/
#endif
