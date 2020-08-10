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
		{"__builtin_alloca",     {.kind = DECLBUILTIN, .builtin = BUILTINALLOCA}},
		{"__builtin_constant_p", {.kind = DECLBUILTIN, .builtin = BUILTINCONSTANTP}},
		{"__builtin_expect",     {.kind = DECLBUILTIN, .builtin = BUILTINEXPECT}},
		{"__builtin_inff",       {.kind = DECLBUILTIN, .builtin = BUILTININFF}},
		{"__builtin_nanf",       {.kind = DECLBUILTIN, .builtin = BUILTINNANF}},
		{"__builtin_offsetof",   {.kind = DECLBUILTIN, .builtin = BUILTINOFFSETOF}},
		{"__builtin_types_compatible_p",
			{.kind = DECLBUILTIN, .builtin = BUILTINTYPESCOMPATIBLEP}},
		{"__builtin_va_arg",     {.kind = DECLBUILTIN, .builtin = BUILTINVAARG}},
		{"__builtin_va_copy",    {.kind = DECLBUILTIN, .builtin = BUILTINVACOPY}},
		{"__builtin_va_end",     {.kind = DECLBUILTIN, .builtin = BUILTINVAEND}},
		{"__builtin_va_list",    {.kind = DECLTYPE, .type = &typevalist}},
		{"__builtin_va_start",   {.kind = DECLBUILTIN, .builtin = BUILTINVASTART}},
		{"bool",   {.kind = DECLTYPE, .type = &typebool}},
		{"char",   {.kind = DECLTYPE, .type = &typechar}},
		{"f32",    {.kind = DECLTYPE, .type = &typef32}},
		{"f64",    {.kind = DECLTYPE, .type = &typef64}},
		{"i8",     {.kind = DECLTYPE, .type = &typei8}},
		{"i16",    {.kind = DECLTYPE, .type = &typei16}},
		{"i32",    {.kind = DECLTYPE, .type = &typei32}},
		{"i64",    {.kind = DECLTYPE, .type = &typei64}},
		{"u8",     {.kind = DECLTYPE, .type = &typeu8}},
		{"u16",    {.kind = DECLTYPE, .type = &typeu16}},
		{"u32",    {.kind = DECLTYPE, .type = &typeu32}},
		{"u64",    {.kind = DECLTYPE, .type = &typeu64}},
	};
	struct builtin *b;

	for (b = builtins; b < builtins + LEN(builtins); ++b)
		scopeputdecl(&filescope, b->name, &b->decl);
	scopeputdecl(&filescope, "rune", mkdecl(DECLTYPE, targ->typerune, LINKNONE, QUALNONE));
	scopeputdecl(&filescope, "int", mkdecl(DECLTYPE, targ->typeint, LINKNONE, QUALNONE));
	scopeputdecl(&filescope, "uint", mkdecl(DECLTYPE, targ->typeuint, LINKNONE, QUALNONE));
	scopeputdecl(&filescope, "long", mkdecl(DECLTYPE, targ->typelong, LINKNONE, QUALNONE));
	scopeputdecl(&filescope, "ulong", mkdecl(DECLTYPE, targ->typeulong, LINKNONE, QUALNONE));
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
