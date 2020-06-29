g(...) void;
f() void {
	static c const unsigned char = 0;
	g(c);
	g(~c);
}
