typedef struct {
	int a, b, c;
	char etc[];
} a;

typedef struct {
	const int d, e, f;
	char etc[];
} b;

struct c {
	int g, h, i;
	char blah[];
};

void f(void) {
	struct c d = {0};
	a e = d;
	b f = e;
	struct c g = f;
}

