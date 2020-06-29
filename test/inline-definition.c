extern f() void;

/*
f is not an inline definition, due to the preceeding declaration,
so we *should* emit an external definition for it.
*/
inline f() void {}
