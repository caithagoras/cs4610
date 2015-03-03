#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#define MAXL 62

typedef struct Vector_t {
  void** v;
  int size;
  int capacity;
} Vector;

void initialize_vector(Vector *v){
  v->size = 0;
  v->capacity = 10;
  v->v = (void**)malloc(sizeof(void*)*v->capacity);
}

void push(Vector *v, void *s) {
  if (v->size == v->capacity) {
    v->capacity *= 2;
    void **tmp = (void**)malloc(sizeof(void*)*v->capacity);
    
    int i;
    for (i = 0; i<v->size; i++)
      tmp[i] = v->v[i];

    v->v = tmp;
  }
  
  v->size++;
  v->v[v->size-1] = s;
}

int find_string(Vector *v, char *s) {
  int i=0;
  for (i=0; i<v->size; i++)
    if (strcmp((char*)(v->v[i]), s) == 0)
      return i;
  return -1;
}

int remove_new_line(char *s) {
  char *c;
  if ((c = strchr(s, '\n')) != NULL)
    *c = '\0';
  if ((c = strchr(s, '\r')) != NULL)
    *c = '\0';
}

int main() {
  Vector graph;
  Vector tasks;
  Vector indeg;
  Vector order;
  initialize_vector(&graph);
  initialize_vector(&tasks);
  initialize_vector(&indeg);
  initialize_vector(&order);

  char line1[MAXL], line2[MAXL];
  while (fgets(line1, MAXL, stdin) && fgets(line2, MAXL, stdin)) {
    remove_new_line(line1);
    remove_new_line(line2);
    
    int i1, i2;
    if (find_string(&tasks, line1) == -1) {
      push(&tasks, strdup(line1));
      push(&indeg, malloc(sizeof(int)));
      push(&graph, malloc(sizeof(Vector)));
      *((int*)indeg.v[indeg.size-1]) = 0;
      initialize_vector((Vector*)graph.v[graph.size-1]);
    }
    if (find_string(&tasks, line2) == -1) {
      push(&tasks, strdup(line2));
      push(&indeg, malloc(sizeof(int)));
      push(&graph, malloc(sizeof(Vector)));
      *((int*)indeg.v[indeg.size-1]) = 0;
      initialize_vector((Vector*)graph.v[graph.size-1]);
    }

    i1 = find_string(&tasks, line1);
    i2 = find_string(&tasks, line2);

    *((int*)indeg.v[i1]) += 1;
    
    int *edge = malloc(sizeof(int));
    *edge = i1;
    push((Vector*)graph.v[i2], edge);
  }

  while (1) {
    int next_node = -1;
    int i;
    for (i=0; i<indeg.size; i++) {
      if (*((int*)indeg.v[i]) == 0 && (next_node == -1 || strcmp((char*)tasks.v[i],(char*)tasks.v[next_node])<0))
        next_node = i;
    }

    if (next_node == -1) 
      break;

    push(&order, tasks.v[next_node]);
    *((int*)indeg.v[next_node]) = -1;

    Vector* edges = (Vector*)graph.v[next_node];
    for (i=0; i<edges->size; i++) {
      int node = *((int*)edges->v[i]);
      *((int*)indeg.v[node]) -= 1;
    }
  }

  if (order.size != indeg.size)
    printf("cycle\n");
  else {
    int i;
    for (i=0; i<order.size; i++)
      printf("%s\n", (char*)order.v[i]);
  }
}
