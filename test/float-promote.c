g1(...) void;
g2(_ int, ...) void;
g3(_ float) void;
f() void {
	g1(1.0f);
	g2(0, 1.0f);
	g3(1.0f);
}
