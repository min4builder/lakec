g(_ int) void;
f() void {
	auto i int;
	for (i = 0; i < 10; ++i)
		g(i);
}
