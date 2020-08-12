define f(a, args...) {
	args + a, # args
}
f(abc, 1, (2, 3), 4)
