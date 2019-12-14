
#ifndef _LIST_H
#define _LIST_H

struct node_t {
	int index;
	int x;                 //busy or idle
	double  t; 			   //service time
	int class;			   // class1 or 2
	struct node_t *next;
};

struct node_t *alloc_node(void);
void free_node(struct node_t *);
void free_all_nodes(struct node_t **);

void insert_after_node(struct node_t *, struct node_t **);
struct node_t *remove_after_node(struct node_t **);

void clear_node(int index, struct node_t **pnext);
void insert_node(int classe ,double temp , struct node_t **phead);
#endif
