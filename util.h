struct list {
	struct list *prev, *next;
};

struct array {
	void *val;
	size_t len, cap;
};

struct mapkey {
	uint64_t hash;
	const char *str;
	size_t len;
};

struct treenode {
	uint64_t key;
	void *child[2];
	int height;
	_Bool new;  /* set by treeinsert if this node was newly allocated */
};

extern char *argv0;

#define LEN(a) (sizeof(a) / sizeof((a)[0]))
#define ALIGNDOWN(x, n) ((x) & -(n))
#define ALIGNUP(x, n) ALIGNDOWN((x) + (n) - 1, n)

void warn(const char *, ...);
_Noreturn void fatal(const char *fmt, ...);

void *reallocarray(void *, size_t, size_t);
void *xreallocarray(void *, size_t, size_t);
void *xmalloc(size_t);

char *progname(char *, char *);

void listinsert(struct list *, struct list *);
void listremove(struct list *);
#define listelement(list, type, member) ((type *)((char *)list - offsetof(type, member)))

void *arrayadd(struct array *, size_t);
void arrayaddptr(struct array *, void *);
void arrayaddbuf(struct array *, const void *, size_t);
void *arraylast(struct array *, size_t);
#define arrayforeach(a, m) for (m = (a)->val; m != (void *)((char *)(a)->val + (a)->len); ++m)

/* map */

void mapkey(struct mapkey *, const char *, size_t);
struct map *mkmap(size_t);
void delmap(struct map *, void(void *));
void **mapput(struct map *, struct mapkey *);
void *mapget(struct map *, struct mapkey *);

/* tree */

void *treeinsert(void **, uint64_t, size_t);
