struct a(x, y int);

.+(x, y struct a) struct a {
	return [struct a]{ x.x + y.x, x.y + y.y };
}

f() void {
	auto x struct a = { 6, 6 };
	auto y struct a = { 7, 7 };
	auto z = x + y;
}

