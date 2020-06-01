extern a = 25;
int a;

extern b = (unsigned char) 26LL;
unsigned char b;

int f(int **d) {
	const e = *d;
	auto f = *e;
	return f;
}

extern c = f;
int (*c)(int **);

