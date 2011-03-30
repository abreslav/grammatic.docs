#define DEFLIST(name, type) struct name { \
            struct name *next; \
            type data; \
};

DEFLIST(char*, StrList);

int main() {
  return 1;
}
