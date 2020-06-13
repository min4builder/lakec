struct a { int x, y; };

struct a .+(struct a x, struct a y) {
	return (struct a) { x.x + y.x, x.y + y.y };
}

void f() {
	struct a x = { 6, 6 };
	struct a y = { 7, 7 };
	struct a z = x + y;
}

