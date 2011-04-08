#include <stdio.h>

#define _DEFLIST(name, type) typedef struct _##name##List name##List; struct _##name##List { \
	name##List *next; \
	type data; \
};

#define DEFLIST(type) _DEFLIST(type, type)

#define INT int

_DEFLIST(Int, int)

#define LIST(name) name##List

int main() {
  IntList l;
  l.data = 1;
  printf("%d", l.data);
}        
