#include <stdio.h>

#define _DEFLIST(name, type) struct name##List { \
	struct name##List *next; \
	type data; \
};

#define DEFLIST(type) _DEFLIST(type, type)

#define INT int

DEFLIST(Int, int)

#define LIST(name) struct name##List

int main() {
  LIST(INT) l;
  l.data = 1;
  printf("%d", l.data);
}        
