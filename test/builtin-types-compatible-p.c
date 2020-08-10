x = __builtin_types_compatible_p(uint, enum(A));
y = __builtin_types_compatible_p(const int, int);  /* qualifiers are ignored */
z = __builtin_types_compatible_p(*int, *uint);
