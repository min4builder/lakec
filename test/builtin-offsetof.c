struct s (
	a [5]struct (
		b union (
			z float,
			c *char,
		),
	),
);
x = __builtin_offsetof(struct s, a[2].b.c);
