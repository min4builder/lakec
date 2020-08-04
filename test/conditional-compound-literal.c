main() int {
	auto x = 0;
	auto p = 0 ? 0 : &[int](x);
	return *p;
}
