x = __builtin_types_compatible_p(unsigned, enum(A));
y = __builtin_types_compatible_p(const int, int);  /* qualifiers are ignored */
z = __builtin_types_compatible_p(*int, *unsigned);
