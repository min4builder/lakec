/* C11 6.7.3p9 - type qualifiers on array type qualify the element type */
typedef T [2]int;
f(x const T) void {
	x = 0;
}
