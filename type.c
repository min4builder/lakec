#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
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

struct type typevoid = {.kind = TYPEVOID, .prop = PROPOBJECT, .incomplete = true};

struct type typenoreturn = {.kind = TYPENORETURN, .repr = &i8, .prop = PROPNONE, .incomplete = true};

struct type typebool = INTTYPE(TYPEBOOL, 1, &i8, false, 0);

struct type typechar = INTTYPE(TYPECHAR, 1, &i8, true, PROPCHAR);

struct type typei8   = INTTYPE(TYPECHAR, 1, &i8, true, 0);
struct type typeu8   = INTTYPE(TYPECHAR, 1, &i8, false, 0);

struct type typei16  = INTTYPE(TYPESHORT, 2, &i16, true, 0);
struct type typeu16  = INTTYPE(TYPESHORT, 2, &i16, false, 0);

struct type typei32  = INTTYPE(TYPEINT, 4, &i32, true, 0);
struct type typeu32  = INTTYPE(TYPEINT, 4, &i32, false, 0);

struct type typei64  = INTTYPE(TYPELLONG, 8, &i64, true, 0);
struct type typeu64  = INTTYPE(TYPELLONG, 8, &i64, false, 0);

struct type typef32  = FLTTYPE(TYPEFLOAT, 4, &f32);
struct type typef64  = FLTTYPE(TYPEDOUBLE, 8, &f64);

struct type typevalist = {
	.kind = TYPESTRUCT, .size = 32, .align = 8,
	.prop = PROPOBJECT|PROPAGGR,
};
struct type typevalistptr = {
	.kind = TYPEPOINTER, .size = 8, .align = 8, .repr = &i64, .base = &typevalist.gen,
	.prop = PROPOBJECT|PROPDERIVED|PROPSCALAR,
};
struct type typevalistmutptr = {
	.kind = TYPEPOINTER, .size = 8, .align = 8, .repr = &i64, .base = &typevalist.gen,
	.qual = QUALMUT, .prop = PROPOBJECT|PROPDERIVED|PROPSCALAR,
};

struct type *
mktype(enum typekind kind, enum typeprop prop)
{
	struct type *t;

	t = xmalloc(sizeof(*t));
	t->kind = kind;
	t->prop = prop;
	t->size = 0;
	t->align = 0;
	t->repr = 0;
	t->base = 0;
	t->qual = QUALNONE;
	t->incomplete = false;

	return t;
}

struct type *
mkapplytype(struct typegen *base, int nvars, struct typegen **vars)
{
	struct type *t;

	t = mktype(TYPEAPPLY, PROPDERIVED);
	t->base = base;
	t->apply.val = NULL;
	t->apply.nvars = nvars;
	t->apply.vars = vars;

	return t;
}

struct type *
mkpointertype(struct typegen *base, enum typequal qual)
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
mkarraytype(struct typegen *base, enum typequal qual, uint64_t len)
{
	struct type *t;

	t = mktype(TYPEARRAY, PROPOBJECT|PROPDERIVED|PROPAGGR);
	t->base = base;
	t->qual = qual;
	t->array.length = len;
	t->incomplete = !len;
	if (t->base) {
		t->align = typeeval(t->base)->align;
		t->size = typeeval(t->base)->size * len;  // XXX: overflow?
	}

	return t;
}

bool
typecast(struct typegen *type1, struct typegen *type2)
{
	struct type *t1, *t2;

	t1 = typeeval(type1);
	t2 = typeeval(type2);

	while (t1->kind == TYPEVAR && !t1->var.isrigid) {
		if (*t1->var.position == t1) {
			*t1->var.position = t2;
			return true;
		}
		t1 = *t1->var.position;
	}

	while (t2->kind == TYPEVAR && !t2->var.isrigid) {
		if (*t2->var.position == t2) {
			*t2->var.position = t1;
			return true;
		}
		t2 = *t2->var.position;
	}

	if (t1 == &typenoreturn || t2 == &typenoreturn)
		return true;
	if (t1->prop & PROPSCALAR && t2->kind == TYPEBOOL)
		return true;
	if (t1->prop & (PROPINT | PROPFLOAT) && t2->prop & PROPFLOAT)
		return true;
	if (t1->prop & PROPINT && t2->prop & PROPINT)
		return true;
	if (t1->kind == TYPEARRAY && t2->kind == TYPEARRAY) {
		return (t1->array.length == t2->array.length || !t2->array.length)
		    && typeequal(t1->base, t2->base);
	}
	if (t1->kind == TYPEPOINTER && t2->kind == TYPEPOINTER)
		return (t1->qual & t2->qual) == t1->qual && typeequal(t1->base, t2->base);
	if (t2->kind == TYPEVOID)
		return true;
	return false;
}

bool
typeequal(struct typegen *type1, struct typegen *type2)
{
	struct param *p1, *p2;
	struct member *m1, *m2;
	struct type *t1, *t2;
	int i;

	t1 = (struct type *) type1;
	t2 = (struct type *) type2;

	while (t1->kind == TYPEVAR && !t1->var.isrigid && *t1->var.position != t1)
		t1 = *t1->var.position;
	while (t2->kind == TYPEVAR && !t2->var.isrigid && *t2->var.position != t2)
		t2 = *t2->var.position;

	if (t1 == t2)
		return true;
	if (t1->kind != t2->kind || t1->size != t2->size)
		return false;
	if (t1->prop & PROPINT)
		return t1->basic.issigned == t2->basic.issigned;
	if (t1->prop & PROPDERIVED && t1->qual != t2->qual)
		return false;

	switch (t1->kind) {
	case TYPEPOINTER:
		return typeequal(t1->base, t2->base);
	case TYPEARRAY:
		return t1->array.length == t2->array.length
			&& typeequal(t1->base, t2->base);
	case TYPESTRUCT:
	case TYPEUNION:
		if (t1->qual != t2->qual)
			return false;
		for (m1 = t1->structunion.members, m2 = t2->structunion.members; m1 && m2; m1 = m1->next, m2 = m2->next) {
			if (!(m1->qual == m2->qual && typeequal(m1->type, m2->type)))
				return false;
		}
		if (m1 || m2)
			return false;
		return true;
	case TYPEFUNC:
		if (t1->func.isvararg != t2->func.isvararg)
			return false;
		for (p1 = t1->func.params, p2 = t2->func.params; p1 && p2; p1 = p1->next, p2 = p2->next) {
			if (p1->qual != p2->qual || !typeequal(p1->type, p2->type))
				return false;
		}
		if (p1 || p2)
			return false;
		return typeequal(t1->base, t2->base);
	case TYPEPARAM:
		return t1->param.nvars == t2->param.nvars && typeequal(t1->base, t2->base);
	case TYPEAPPLY:
		if (!t1->apply.vars || !t2->apply.vars)
			return typeequal(t1->base, t2->base);
		for (i = 0; i < t1->apply.nvars; i++) {
			if (t1->apply.vars[i] != t2->apply.vars[i]
			&& (!t1->apply.vars[i] || !t2->apply.vars[i]))
				return false;
			if (!typeequal(t1->apply.vars[i], t2->apply.vars[i]))
				return false;
		}
		return typeequal(t1->base, t2->base);
	}
	return false;
}

struct typegen *
typecomposite(struct typegen *t1, struct typegen *t2)
{
	// XXX: implement 6.2.7
	// XXX: merge with typecompatible?
	if (t1 == &typenoreturn.gen)
		return t2;
	return t1;
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

struct typegen *
typepromote(struct typegen *type, unsigned width)
{
	struct type *t = typeeval(type);

	if (t == &typef32)
		return &typef64.gen;
	if (t->prop & PROPINT && (typerank(t) <= typerank(targ->typeint) || width <= targ->typeint->size * 8)) {
		if (width == -1)
			width = t->size * 8;
		return width - t->basic.issigned < targ->typeint->size * 8 ? &targ->typeint->gen : &targ->typeuint->gen;
	}
	return type;
}

struct typegen *
typecommonreal(struct typegen *type1, unsigned w1, struct typegen *type2, unsigned w2)
{
	struct type *t1, *t2, *tmp;

	t1 = typeeval(type1);
	t2 = typeeval(type2);

	if (t1 == &typenoreturn) {
		assert(t2->prop & PROPREAL);
		return &t2->gen;
	}
	if (t2 == &typenoreturn) {
		assert(t1->prop & PROPREAL);
		return &t1->gen;
	}
	assert(t1->prop & PROPREAL && t2->prop & PROPREAL);
	if (t1 == &typef64 || t2 == &typef64)
		return &typef64.gen;
	if (t1 == &typef32 || t2 == &typef32)
		return &typef32.gen;
	t1 = (struct type *) typepromote(&t1->gen, w1);
	t2 = (struct type *) typepromote(&t2->gen, w2);
	if (t1 == t2)
		return &t1->gen;
	if (t1->basic.issigned == t2->basic.issigned)
		return typerank(t1) > typerank(t2) ? &t1->gen : &t2->gen;
	if (t1->basic.issigned) {
		tmp = t1;
		t1 = t2;
		t2 = tmp;
	}
	if (typerank(t1) >= typerank(t2))
		return &t1->gen;
	if (t1->size < t2->size)
		return &t2->gen;
	if (t2 == targ->typelong)
		return &targ->typeulong->gen;
	if (t2 == &typei64)
		return &typeu64.gen;
	fatal("internal error; could not find common real type");
}

struct member *
typemember(struct type *t, const char *name, uint64_t *offset)
{
	struct member *m, *sub;

	assert(t->kind == TYPESTRUCT || t->kind == TYPEUNION);
	for (m = t->structunion.members; m; m = m->next) {
		if (strcmp(m->name, "_")) {
			if (strcmp(m->name, name) == 0) {
				*offset += m->offset;
				return m;
			}
		} else if (typeeval(m->type)->kind == TYPESTRUCT || typeeval(m->type)->kind == TYPEUNION) {
			sub = typemember(typeeval(m->type), name, offset);
			if (sub) {
				*offset += m->offset;
				return sub;
			}
		}
	}
	return NULL;
}

static struct type *
instantiate(struct type *d, struct type *t, struct type **vars)
{
	struct param *p, **p2;
	struct member *m, **m2;
	struct type *newt;
	int i;

	switch (t->kind) {
	case TYPEPOINTER:
		newt = mkpointertype(t->base, t->qual);
		goto derived;
	case TYPEARRAY:
		newt = mkarraytype(t->base, t->qual, t->array.length);
		goto derived;
	case TYPESTRUCT:
	case TYPEUNION:
		newt = mktype(t->kind, t->prop);
		newt->repr = t->repr;
		newt->size = 0;
		newt->align = t->align;
		newt->qual = t->qual;
		newt->incomplete = t->incomplete;
		newt->structunion.members = NULL;
		m2 = &newt->structunion.members;
		for (m = t->structunion.members; m; m = m->next) {
			*m2 = xmalloc(sizeof(**m2));
			(*m2)->name = m->name;
			(*m2)->type = &instantiate(d, (struct type *) m->type, vars)->gen;
			(*m2)->qual = m->qual;
			newt->size = ALIGNUP(newt->size, ((struct type *) (*m2)->type)->align);
			if (t->kind == TYPESTRUCT) {
				(*m2)->offset = newt->size;
				newt->size += ((struct type *) (*m2)->type)->size;
			} else {
				(*m2)->offset = 0;
				if (newt->size < ((struct type *) (*m2)->type)->size)
					newt->size = ((struct type *) (*m2)->type)->size;
			}
			/* TODO handle bitfields */
			if (((struct type *) (*m2)->type)->align < newt->align)
				newt->align = ((struct type *) (*m2)->type)->align;
			(*m2)->bits = m->bits;
			(*m2)->next = NULL;
			m2 = &(*m2)->next;
		}
		newt->size = ALIGNUP(newt->size, newt->align);
		return newt;
	case TYPEFUNC:
		newt = mktype(TYPEFUNC, t->prop);
		newt->func.isvararg = t->func.isvararg;
		p2 = &newt->func.params;
		for (p = t->func.params; p; p = p->next) {
			*p2 = mkparam(p->name, &instantiate(d, (struct type *) p->type, vars)->gen, p->qual);
			p2 = &(*p2)->next;
		}
		goto derived;
	case TYPEAPPLY:
		newt = mktype(TYPEAPPLY, t->prop);
		*newt = *t;
		newt->apply.vars = xreallocarray(NULL, sizeof(*newt->apply.vars), ((struct type *) t->base)->apply.nvars);
		for (i = 0; i < ((struct type *) t->base)->apply.nvars; i++)
			newt->apply.vars[i] = &instantiate(d, (struct type *) t->apply.vars[i], vars)->gen;
		return newt;
	default:
		newt = t;
		if (t->kind == TYPEVAR) {
			if (t->var.parent == d) {
				if (!vars[t->var.num]) {
					newt = mktype(TYPEVAR, t->prop);
					*newt = *t;
					newt->var.isrigid = false;
					vars[t->var.num] = newt;
					newt->var.position = &vars[t->var.num];
					return newt;
				} else {
					newt = vars[t->var.num];
				}
			}
		}
		if (newt->prop & PROPDERIVED) {
			newt = mktype(newt->kind, newt->prop);
			*newt = *newt;
			goto derived;
		}
		return newt;
	derived:
		newt->base = &instantiate(d, (struct type *) t->base, vars)->gen;
		return newt;
	}
}

struct type *
typeeval(struct typegen *type)
{
	struct type *t = (struct type *) type;

	while (t->kind == TYPEAPPLY || t->kind == TYPEVAR) {
		if (t->kind == TYPEAPPLY) {
			if (!t->apply.val) {
				if (((struct type *) t->base)->kind != TYPEPARAM)
					error(&t->apply.loc, "type is not parametric");
				if (!t->apply.vars) {
					t->apply.nvars = ((struct type *) t->base)->param.nvars;
					t->apply.vars = xreallocarray(NULL, sizeof(*t->apply.vars), t->apply.nvars);
				}
				if (((struct type *) t->base)->param.nvars != t->apply.nvars)
					error(&t->apply.loc, "wrong number of type arguments");
				t->apply.val = instantiate((struct type *) t->base, (struct type *) ((struct type *) t->base)->base, (struct type **) t->apply.vars);
			}
			t = t->apply.val;
		} else {
			if (t->var.isrigid || *t->var.position != t)
				break;
			t = *t->var.position;
		}
	}
	return t;
}

struct param *
mkparam(char *name, struct typegen *t, enum typequal tq)
{
	struct param *p;

	p = xmalloc(sizeof(*p));
	p->name = name;
	p->type = t;
	p->qual = tq;
	p->next = NULL;

	return p;
}
