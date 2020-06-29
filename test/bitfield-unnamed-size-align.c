struct s (
	_ : 8,
	c char,
);
union u (
	_ : 8,
	c char,
);
s1 = sizeof(struct s);
s2 = _Alignof(struct s);
u1 = sizeof(union u);
u2 = _Alignof(union u);
