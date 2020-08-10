s1 struct(x : 31);
s2 struct(x : 32);
s3 struct(x : 33);
c1 = __builtin_types_compatible_p(__typeof__(+s1.x), int);
c2 = __builtin_types_compatible_p(__typeof__(+s2.x), uint);
c3 = __builtin_types_compatible_p(__typeof__(+s3.x), ulong);
