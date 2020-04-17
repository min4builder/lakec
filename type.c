#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "util.h"
#include "cc.h"

#define INTTYPE(k, n, r, s, p) { \
	.kind = k, .size = n, .align = n, .repr = r, .basic.issigned = s, \
	.prop = PROPOBJECT|PROPSCALAR|PROPARITH|PROPREAL|PROPINT|p, \
}
#define FLTTYPE(k, n, r) { \
	.kind = k, .size = n, .align = n, .repr = r, \
	.prop = PROPOBJECT|PROPSCALAR|PROPARITH|PROPREAL|PROPFLOAT, \
}

struct type typevoid    = {.kind = TYPEVOID, .prop = PROPOBJECT, .incomplete = true};

struct type typebool    = INTTYPE(TYPEBOOL, 1, &i8, false, 0);

struct type typechar    = INTTYPE(TYPECHAR, 1, &i8, true, PROPCHAR);
struct type typeschar   = INTTYPE(TYPECHAR, 1, &i8, true, PROPCHAR);
struct type typeuchar   = INTTYPE(TYPECHAR, 1, &i8, false, PROPCHAR);

struct type typeshort   = INTTYPE(TYPESHORT, 2, &i16, true, 0);
struct type typeushort  = INTTYPE(TYPESHORT, 2, &i16, false, 0);

struct type typeint     = INTTYPE(TYPEINT, 4, &i32, true, 0);
struct type typeuint    = INTTYPE(TYPEINT, 4, &i32, false, 0);

struct type typelong    = INTTYPE(TYPELONG, 8, &i64, true, 0);
struct type typeulong   = INTTYPE(TYPELONG, 8, &i64, false, 0);

struct type typellong   = INTTYPE(TYPELLONG, 8, &i64, true, 0);
struct type typeullong  = INTTYPE(TYPELLONG, 8, &i64, false, 0);

struct type typefloat   = FLTTYPE(TYPEFLOAT, 4, &f32);
struct type typedouble  = FLTTYPE(TYPEDOUBLE, 8, &f64);
struct type typeldouble = FLTTYPE(TYPELDOUBLE, 16, NULL);  // XXX: not supported by qbe

static struct type typevaliststruct = {
	.kind = TYPESTRUCT, .size = 32, .align = 8,
	.prop = PROPOBJECT|PROPAGGR,
};
struct type typevalist = {
	.kind = TYPEARRAY, .size = 32, .align = 8, .array = {1}, .base = &typevaliststruct,
	.prop = PROPOBJECT|PROPDERIVED|PROPAGGR,
};
struct type typevalistptr = {
	.kind = TYPEPOINTER, .size = 8, .align = 8, .repr = &i64, .base = &typevaliststruct,
	.prop = PROPOBJECT|PROPDERIVED|PROPSCALAR,
};

struct type *
mktype(enum typekind kind, enum typeprop prop)
{
	struct type *t;

	t = xmalloc(sizeof(*t));
	t->kind = kind;
	t->prop = prop;
	t->incomplete = 0;

	return t;
}

struct type *
mkpointertype(struct type *base, enum typequal qual)
{
	struct type *t;

	t = mktype(TYPEPOINTER, PROPOBJECT|PROPDERIVED|PROPSCALAR);
	t->base = base;
	t->qual = qual;
	t->size = 8;
	t->align = 8;
	t->repr = &i64;

	return t;
}

struct type *
mkarraytype(struct type *base, enum typequal qual, uint64_t len)
{
	struct type *t;

	t = mktype(TYPEARRAY, PROPOBJECT|PROPDERIVED|PROPAGGR);
	t->base = base;
	t->qual = qual;
	t->array.length = len;
	t->incomplete = !len;
	if (t->base) {
		t->align = t->base->align;
		t->size = t->base->size * len;  // XXX: overflow?
	}

	return t;
}

static int
typerank(struct type *t)
{
	assert(t->prop & PROPINT);
	switch (t->kind) {
	case TYPEBOOL:  return 1;
	case TYPECHAR:  return 2;
	case TYPESHORT: return 3;
	case TYPEENUM:
	case TYPEINT:   return 4;
	case TYPELONG:  return 5;
	case TYPELLONG: return 6;
	default:
		fatal("internal error; unhandled integer type");
	}
}

bool
typecompatible(struct type *t1, struct type *t2)
{
	struct type *tmp;
	struct param *p1, *p2;

	if (t1 == t2)
		return true;
	if (t1->kind != t2->kind) {
		/* enum types are compatible with 'int', but not with
		   each other (unless they are the same type) */
		return (t1->kind == TYPEENUM && t2->kind == TYPEINT ||
		        t1->kind == TYPEINT && t2->kind == TYPEENUM) &&
		       t1->basic.issigned == t2->basic.issigned;
	}
	switch (t1->kind) {
	case TYPEPOINTER:
		goto derived;
	case TYPEARRAY:
		if (t1->array.length && t2->array.length && t1->array.length != t2->array.length)
			return false;
		goto derived;
	case TYPEFUNC:
		if (!t1->func.isprototype) {
			if (!t2->func.isprototype)
				return true;
			tmp = t1, t1 = t2, t2 = tmp;
		}
		if (t1->func.isvararg != t2->func.isvararg)
			return false;
		if (!t2->func.paraminfo) {
			for (p1 = t1->func.params; p1; p1 = p1->next) {
				if (!typecompatible(p1->type, typepromote(p1->type, -1)))
					return false;
			}
			return true;
		}
		for (p1 = t1->func.params, p2 = t2->func.params; p1 && p2; p1 = p1->next, p2 = p2->next) {
			tmp = t2->func.isprototype ? p2->type : typepromote(p2->type, -1);
			if (!typecompatible(p1->type, tmp))
				return false;
		}
		if (p1 || p2)
			return false;
		goto derived;
	derived:
		return t1->qual == t2->qual && typecompatible(t1->base, t2->base);
	}
	return false;
}

bool
typeconvertible(struct type *t1, struct type *t2)
{
	if (t1->prop & PROPSCALAR && t2->kind == TYPEBOOL)
		return true;
	if (t1->prop & (PROPINT | PROPFLOAT) && t2->prop & (PROPINT | PROPFLOAT))
		return true;
	if (t1->kind == TYPEPOINTER && t2->kind == TYPEPOINTER) {
		if (t1->base->kind == TYPEVOID || t2->base->kind == TYPEVOID)
			return true;
		if ((t1->qual & t2->qual) == t1->qual && typesame(t1->base, t2->base))
			return true;
	}
	if (t1->prop & PROPINT && t2->kind == TYPEPOINTER)
		return true;
	if (t1->kind == TYPEARRAY && t2->kind == TYPEPOINTER && typesame(t1->base, t2->base))
		return true;
	if (t2->kind == TYPEVOID)
		return true;
	return false;
}

bool
typesame(struct type *t1, struct type *t2)
{
	struct param *p1, *p2;

	if (t1 == t2)
		return true;
	if (t1->kind != t2->kind || t1->size != t2->size)
		return false;
	if (t1->prop & PROPINT)
		return t1->basic.issigned == t2->basic.issigned;
	if (t1->qual != t2->qual)
		return false;

	switch (t1->kind) {
	case TYPEPOINTER:
		return typesame(t1->base, t2->base);
	case TYPEARRAY:
		return t1->array.length == t2->array.length
			&& typesame(t1->base, t2->base);
	case TYPEFUNC:
		if (t1->func.isprototype != t2->func.isprototype ||
		    t1->func.isvararg != t2->func.isvararg ||
		    t1->func.isnoreturn != t2->func.isnoreturn ||
		    t1->func.paraminfo != t2->func.paraminfo)
			return false;
		if (!t1->func.paraminfo)
			return typesame(t1->base, t2->base);
		for (p1 = t1->func.params, p2 = t2->func.params; p1 && p2; p1 = p1->next, p2 = p2->next) {
			if (p1->qual != p2->qual || !typesame(p1->type, p2->type))
				return false;
		}
		if (p1 || p2)
			return false;
		return typesame(t1->base, t2->base);
	}
	return false;
}

struct type *
typecomposite(struct type *t1, struct type *t2)
{
	// XXX: implement 6.2.7
	// XXX: merge with typecompatible?
	return t1;
}

struct type *
typepromote(struct type *t, unsigned width)
{
	if (t == &typefloat)
		return &typedouble;
	if (t->prop & PROPINT && (typerank(t) <= typerank(&typeint) || width <= typeint.size * 8)) {
		if (width == -1)
			width = t->size * 8;
		return width - t->basic.issigned < typeint.size * 8 ? &typeint : &typeuint;
	}
	return t;
}

struct type *
typecommonreal(struct type *t1, unsigned w1, struct type *t2, unsigned w2)
{
	struct type *tmp;

	assert(t1->prop & PROPREAL && t2->prop & PROPREAL);
	if (t1 == &typeldouble || t2 == &typeldouble)
		return &typeldouble;
	if (t1 == &typedouble || t2 == &typedouble)
		return &typedouble;
	if (t1 == &typefloat || t2 == &typefloat)
		return &typefloat;
	t1 = typepromote(t1, w1);
	t2 = typepromote(t2, w2);
	if (t1 == t2)
		return t1;
	if (t1->basic.issigned == t2->basic.issigned)
		return typerank(t1) > typerank(t2) ? t1 : t2;
	if (t1->basic.issigned) {
		tmp = t1;
		t1 = t2;
		t2 = tmp;
	}
	if (typerank(t1) >= typerank(t2))
		return t1;
	if (t1->size < t2->size)
		return t2;
	if (t2 == &typelong)
		return &typeulong;
	if (t2 == &typellong)
		return &typeullong;
	fatal("internal error; could not find common real type");
}

struct member *
typemember(struct type *t, const char *name, uint64_t *offset)
{
	struct member *m, *sub;

	assert(t->kind == TYPESTRUCT || t->kind == TYPEUNION);
	for (m = t->structunion.members; m; m = m->next) {
		if (m->name) {
			if (strcmp(m->name, name) == 0) {
				*offset += m->offset;
				return m;
			}
		} else {
			sub = typemember(m->type, name, offset);
			if (sub) {
				*offset += m->offset;
				return sub;
			}
		}
	}
	return NULL;
}

struct param *
mkparam(char *name, struct type *t, enum typequal tq)
{
	struct param *p;

	p = xmalloc(sizeof(*p));
	p->name = name;
	p->type = t;
	p->qual = tq;
	p->next = NULL;

	return p;
}
