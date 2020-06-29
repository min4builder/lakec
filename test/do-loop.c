main() int {
	auto x, y = 2, 0;
	do {
		if (x == 1)
			continue;
		++y;
	} while (x--);
	return y != 2;
}
