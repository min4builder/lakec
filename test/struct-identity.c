typedef a struct (
	a, b, c int,
	etc []char,
);

typedef b struct (
	d, e, f const int,
	etc []char,
);

struct c (
	g, h, i int,
	blah []char,
);

f() void {
	auto d *struct c = &[struct c]{0};
	auto e *a = d;
	auto f *b = e;
	auto g *struct c = f;
}

