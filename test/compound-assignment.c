f() void {
	auto x [1]int = {0};
	auto p *int = x;
	*p++ += 1;
}
