extern a = 25;
a int;

extern b = 26->[u8];
b u8;

f(d **int) int {
	auto e = *d;
	return *e;
}

extern c *(_ **int) int = &f;
c *(_ **int) int;

