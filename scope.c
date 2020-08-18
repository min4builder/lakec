#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include "util.h"
#include "cc.h"

struct scope filescope;

void
scopeinit(void)
{
	static struct builtin {
		char *name;
		struct decl decl;
	} builtins[] = {
		{"__builtin_constant_p", {.kind = DECLBUILTIN, .builtin = BUILTINCONSTANTP}},
		{"__builtin_expect",     {.kind = DECLBUILTIN, .builtin = BUILTINEXPECT}},
		{"__builtin_inff",       {.kind = DECLBUILTIN, .builtin = BUILTININFF}},
		{"__builtin_nanf",       {.kind = DECLBUILTIN, .builtin = BUILTINNANF}},
		{"__builtin_types_compatible_p",
			{.kind = DECLBUILTIN, .builtin = BUILTINTYPESCOMPATIBLEP}},
		{"__builtin_va_arg",     {.kind = DECLBUILTIN, .builtin = BUILTINVAARG}},
		{"__builtin_va_copy",    {.kind = DECLBUILTIN, .builtin = BUILTINVACOPY}},
		{"__builtin_va_end",     {.kind = DECLBUILTIN, .builtin = BUILTINVAEND}},
		{"__builtin_va_start",   {.kind = DECLBUILTIN, .builtin = BUILTINVASTART}},
		{"alloca",   {.kind = DECLBUILTIN, .builtin = BUILTINALLOCA}},
		{"offsetof", {.kind = DECLBUILTIN, .builtin = BUILTINOFFSETOF}},
	};
	struct builtin *b;

	for (b = builtins; b < builtins + LEN(builtins); ++b)
		scopeputdecl(&filescope, b->name, &b->decl);

	scopeputtag(&filescope, "__builtin_va_list", &typevalist);
	scopeputtag(&filescope, "bool", &typebool);
	scopeputtag(&filescope, "char", &typechar);
	scopeputtag(&filescope, "f32", &typef32);
	scopeputtag(&filescope, "f64", &typef64);
	scopeputtag(&filescope, "i8", &typei8);
	scopeputtag(&filescope, "i16", &typei16);
	scopeputtag(&filescope, "i32", &typei32);
	scopeputtag(&filescope, "i64", &typei64);
	scopeputtag(&filescope, "u8", &typeu8);
	scopeputtag(&filescope, "u16", &typeu16);
	scopeputtag(&filescope, "u32", &typeu32);
	scopeputtag(&filescope, "u64", &typeu64);
	scopeputtag(&filescope, "rune", targ->typerune);
	scopeputtag(&filescope, "int", targ->typeint);
	scopeputtag(&filescope, "uint", targ->typeuint);
	scopeputtag(&filescope, "long", targ->typelong);
	scopeputtag(&filescope, "ulong", targ->typeulong);
}

struct scope *
mkscope(struct scope *parent)
{
	struct scope *s;

	s = xmalloc(sizeof(*s));
	s->decls = NULL;
	s->tags = NULL;
	s->breaklabel = parent->breaklabel;
	s->continuelabel = parent->continuelabel;
	s->switchcases = parent->switchcases;
	s->switchcond = parent->switchcond;
	s->parent = parent;

	return s;
}

struct scope *
delscope(struct scope *s)
{
	struct scope *parent = s->parent;

	if (s->decls)
		delmap(s->decls, NULL);
	if (s->tags)
		delmap(s->tags, NULL);
	free(s);

	return parent;
}

struct decl *
scopegetdecl(struct scope *s, const char *name, bool recurse)
{
	struct decl *d;
	struct mapkey k;

	mapkey(&k, name, strlen(name));
	do {
		d = s->decls ? mapget(s->decls, &k) : NULL;
		s = s->parent;
	} while (!d && s && recurse);

	return d;
}

struct type *
scopegettag(struct scope *s, const char *name, bool recurse)
{
	struct type *t;
	struct mapkey k;

	mapkey(&k, name, strlen(name));
	do {
		t = s->tags ? mapget(s->tags, &k) : NULL;
		s = s->parent;
	} while (!t && s && recurse);

	return t;
}

void
scopeputdecl(struct scope *s, const char *name, struct decl *d)
{
	struct mapkey k;

	if (!s->decls)
		s->decls = mkmap(32);
	if (name[0] == '_' && name[1] == '\0')
		return;
	mapkey(&k, name, strlen(name));
	*mapput(s->decls, &k) = d;
}

void
scopeputtag(struct scope *s, const char *name, struct type *t)
{
	struct mapkey k;

	if (!s->tags)
		s->tags = mkmap(32);
	if (name[0] == '_' && name[1] == '\0')
		return;
	mapkey(&k, name, strlen(name));
	*mapput(s->tags, &k) = t;
}
