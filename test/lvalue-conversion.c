g(...) void;
f() void {
	static c const u8 = 0;
	g(c);
	g(~c);
}
