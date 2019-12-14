
#include "list.h"
#include <stdio.h>
#include <stdlib.h>

int c;

struct node_t *alloc_node(void)
{
	struct node_t *p;

	p = malloc(sizeof(struct node_t));

	if (p == NULL) {
		fprintf(stderr, "alloc_node: failed.\n");
		exit(EXIT_FAILURE);
	}

	return p;
}

void free_node(struct node_t *p )
{
	free(p);
}

void free_all_nodes(struct node_t **h)
{
	struct node_t *p;

	while (*h != NULL) {
		p = remove_after_node(h);
		free_node(p);
	}
}

void insert_after_node(struct node_t *new, struct node_t **pnext)
{
	new->next = *pnext;
	*pnext = new;
}

struct node_t *remove_after_node(struct node_t **ppos)
{
	struct node_t *r = *ppos;
	*ppos = r->next;
	return r;
}


void clear_node(int index, struct node_t **pnext)
{
    /*
	struct node_t *q = *pnext;
	struct node_t *p = q->next;

	for (p; p != NULL; pnext=&(p->next), p = p->next ) {
		if ((p->t < q->t)) {
			 q=p;
		}
	}
	double t= q->t;

	*/
    struct node_t *p;
    for (p =*pnext; p != NULL; p = p->next) {
        if(p->index==index){
            p->t=200000000000000000; // unachievable
            p->x=0;                 //set to idle
            //p->class=0;

        }

    }


}

void insert_node(int classe,double temp, struct node_t **phead)
{
	struct node_t *new;

	struct node_t *p;
	for (p =*phead; p != NULL; p = p->next) {
		if(p->x ==0 ){
			p->x = 1;//set to busy
			p->class = classe;
			p->t = temp;
			return;
		}

	}
	c++;
	new = alloc_node();
	new->index=c;
	new->x = 1;//set to busy
	new->class = classe;
	new->t = temp;
	insert_after_node(new, phead);
}



