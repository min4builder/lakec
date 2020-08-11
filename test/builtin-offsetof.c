struct s (
	a [5]struct (
		b union (
			z f32,
			c *char,
		),
	),
);
x = offsetof(struct s, a[2].b.c);
