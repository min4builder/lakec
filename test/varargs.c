f(n int, ...) void {
	auto ap __builtin_va_list;

	__builtin_va_start(ap, n);
	while (n) {
		__builtin_va_arg(ap, int);
		__builtin_va_arg(ap, f32);
		__builtin_va_arg(ap, *char);
		--n;
	}
	__builtin_va_end(ap);
}
