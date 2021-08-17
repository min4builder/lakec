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
		{"alloca",   {.kind = DECLBUILTIN, .builtin = BUILTINALLOCA}},
		{"offsetof", {.kind = DECLBUILTIN, .builtin = BUILTINOFFSETOF}},
	};
	struct builtin *b;
	struct decl *d;

	for (b = builtins; b < builtins + LEN(builtins); ++b)
		scopeputdecl(&filescope, b->name, &b->decl);

	d = mkdecl(DECLCONST, &typebool.gen, LINKNONE);
	d->value = mkintconst(typebool.repr, 0);
	scopeputdecl(&filescope, "false", d);
	d = mkdecl(DECLCONST, &typebool.gen, LINKNONE);
	d->value = mkintconst(typebool.repr, 1);
	scopeputdecl(&filescope, "true", d);

	scopeputtag(&filescope, "__builtin_va_list", &typevalist.gen);
	scopeputtag(&filescope, "noreturn", &typenoreturn.gen);
	scopeputtag(&filescope, "bool", &typebool.gen);
	scopeputtag(&filescope, "char", &typechar.gen);
	scopeputtag(&filescope, "f32", &typef32.gen);
	scopeputtag(&filescope, "f64", &typef64.gen);
	scopeputtag(&filescope, "i8", &typei8.gen);
	scopeputtag(&filescope, "i16", &typei16.gen);
	scopeputtag(&filescope, "i32", &typei32.gen);
	scopeputtag(&filescope, "i64", &typei64.gen);
	scopeputtag(&filescope, "u8", &typeu8.gen);
	scopeputtag(&filescope, "u16", &typeu16.gen);
	scopeputtag(&filescope, "u32", &typeu32.gen);
	scopeputtag(&filescope, "u64", &typeu64.gen);
	scopeputtag(&filescope, "rune", &targ->typerune->gen);
	scopeputtag(&filescope, "int", &targ->typeint->gen);
	scopeputtag(&filescope, "uint", &targ->typeuint->gen);
	scopeputtag(&filescope, "long", &targ->typelong->gen);
	scopeputtag(&filescope, "ulong", &targ->typeulong->gen);
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
	s->func = parent->func;
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

struct typegen *
scopegettag(struct scope *s, const char *name, bool recurse)
{
	struct typegen *t;
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
scopeputtag(struct scope *s, const char *name, struct typegen *t)
{
	struct mapkey k;

	if (!s->tags)
		s->tags = mkmap(32);
	if (name[0] == '_' && name[1] == '\0')
		return;
	mapkey(&k, name, strlen(name));
	*mapput(s->tags, &k) = t;
}
