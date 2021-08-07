#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "util.h"
#include "cc.h"

static struct expr *
mkexpr(enum exprkind k, struct typegen *t)
{
	struct expr *e;

	e = xmalloc(sizeof(*e));
	e->qual = QUALNONE;
	e->type = t;
	e->lvalue = false;
	e->kind = k;
	e->next = NULL;

	return e;
}

void
delexpr(struct expr *e)
{
	struct expr *sub;

	switch (e->kind) {
	case EXPRCALL:
		delexpr(e->base);
		while (sub = e->call.args) {
			e->call.args = sub->next;
			delexpr(sub);
		}
		break;
	case EXPRBITFIELD:
	case EXPRINCDEC:
	case EXPRUNARY:
	case EXPRCAST:
		delexpr(e->base);
		break;
	case EXPRBINARY:
		delexpr(e->binary.l);
		delexpr(e->binary.r);
		break;
	case EXPRCOND:
		delexpr(e->base);
		delexpr(e->cond.t);
		delexpr(e->cond.f);
		break;
	/*
	XXX: compound assignment causes some reuse of expressions,
	so we can't free them without risk of a double-free

	case EXPRASSIGN:
		delexpr(e->assign.l);
		delexpr(e->assign.r);
		break;
	*/
	case EXPRCOMMA:
		while (sub = e->base) {
			e->base = sub->next;
			delexpr(sub);
		}
		break;
	}
	free(e);
}

static struct expr *
mkconstexpr(struct typegen *t, uint64_t n)
{
	struct expr *e;

	e = mkexpr(EXPRCONST, t);
	e->constant.i = n;

	return e;
}

static struct expr *
mkunaryexpr(enum tokenkind op, struct expr *base)
{
	struct expr *expr;

	switch (op) {
	case TBAND:
		/*
		Allow struct and union types even if they are not lvalues,
		since we take their address when compiling member access.
		*/
		if (!base->lvalue && typeeval(base->type)->kind != TYPEFUNC && typeeval(base->type)->kind != TYPESTRUCT && typeeval(base->type)->kind != TYPEUNION && typeeval(base->type)->kind != TYPEARRAY)
			error(&tok.loc, "'&' operand is not an lvalue or function designator");
		if (base->kind == EXPRBITFIELD)
			error(&tok.loc, "cannot take address of bit-field");
		expr = mkexpr(EXPRUNARY, &mkpointertype(base->type, base->qual)->gen);
		expr->op = op;
		expr->base = base;
		return expr;
	case TMUL:
		if (typeeval(base->type)->kind != TYPEPOINTER)
			error(&tok.loc, "cannot dereference non-pointer");
		expr = mkexpr(EXPRUNARY, typeeval(base->type)->base);
		expr->qual = typeeval(base->type)->qual;
		expr->lvalue = true;
		expr->op = op;
		expr->base = base;
		return expr;
	}
	/* other unary operators get compiled as equivalent binary ones */
	fatal("internal error: unknown unary operator %d", op);
}

static unsigned
bitfieldwidth(struct expr *e)
{
	if (e->kind != EXPRBITFIELD)
		return -1;
	return typeeval(e->type)->size * 8 - e->bitfield.bits.before - e->bitfield.bits.after;
}

struct expr *
exprpromote(struct expr *e)
{
	struct typegen *t;

	t = typepromote(e->type, bitfieldwidth(e));
	return exprconvert(e, e->qual, t);
}

static struct typegen *
commonreal(struct expr **e1, struct expr **e2)
{
	struct typegen *t;

	t = typecommonreal((*e1)->type, bitfieldwidth(*e1), (*e2)->type, bitfieldwidth(*e2));
	*e1 = exprconvert(*e1, (*e1)->qual, t);
	*e2 = exprconvert(*e2, (*e2)->qual, t);

	return t;
}

static bool
nullpointer(struct expr *e)
{
	if (e->kind != EXPRCONST)
		return false;
	if (!(typeeval(e->type)->prop & PROPINT) && (typeeval(e->type)->kind != TYPEPOINTER || typeeval(e->type)->base != &typevoid.gen))
		return false;
	return e->constant.i == 0;
}

static void
condunify(struct expr *e)
{
	struct type *t, *f;
	enum typequal tq;

	t = typeeval(e->cond.t->type);
	f = typeeval(e->cond.f->type);
	if (t == f) {
		e->type = &t->gen;
	} else if (t->prop & PROPARITH && f->prop & PROPARITH) {
		e->type = commonreal(&e->cond.t, &e->cond.f);
	} else if (t == &typevoid && f == &typevoid) {
		e->type = &typevoid.gen;
	} else if (e->cond.f->type == &typenoreturn.gen) {
		e->type = &t->gen;
	} else if (e->cond.t->type == &typenoreturn.gen) {
		e->type = &f->gen;
	} else {
		e->cond.t = eval(e->cond.t, EVALARITH);
		e->cond.f = eval(e->cond.f, EVALARITH);
		if (nullpointer(e->cond.t) && f->kind == TYPEPOINTER) {
			e->type = &f->gen;
		} else if (nullpointer(e->cond.f) && t->kind == TYPEPOINTER) {
			e->type = &t->gen;
		} else if (t->kind == TYPEPOINTER && f->kind == TYPEPOINTER) {
			tq = t->qual | f->qual;
			t = typeeval(t->base);
			f = typeeval(f->base);
			if (t == &typevoid || f == &typevoid) {
				e->type = &typevoid.gen;
			} else {
				if (!typecast(&t->gen, &f->gen))
					error(&tok.loc, "operands of conditional operator must have compatible types");
				e->type = typecomposite(&t->gen, &f->gen);
			}
			e->type = &mkpointertype(e->type, tq)->gen;
		} else {
			error(&tok.loc, "invalid operands to conditional operator");
		}
	}
}

static struct expr *
mkoverloadexpr(struct decl *d, struct expr *l, struct expr *r)
{
	struct expr *f, *e;

	f = mkexpr(EXPRIDENT, d->type);
	f->qual = d->qual;
	f->ident.decl = d;
	f->lvalue = 0;
	e = mkexpr(EXPRCALL, typeeval(f->type)->base);
	e->base = mkunaryexpr(TBAND, f);
	e->call.args = l;
	e->call.args->next = r;
	e->call.nargs = r ? 2 : 1;

	return e;
}

static struct expr *
mkbinaryexpr(struct scope *s, struct location *loc, enum tokenkind op, struct expr *l, struct expr *r)
{
	struct expr *e;
	struct typegen *t = NULL;
	struct decl *d;
	enum typeprop lp, rp;

	if (s) {
		d = scopegetdecl(s, manglebop(op, l->type, r->type), true);
		if (d)
			return mkoverloadexpr(d, l, r);
	}
	lp = typeeval(l->type)->prop;
	rp = typeeval(r->type)->prop;
	switch (op) {
	case TLOR:
	case TLAND:
		if (!(lp & PROPSCALAR))
			error(loc, "left operand of '%s' operator must be scalar", tokstr[op]);
		if (!(rp & PROPSCALAR))
			error(loc, "right operand of '%s' operator must be scalar", tokstr[op]);
		l = exprconvert(l, l->qual, &typebool.gen);
		r = exprconvert(r, r->qual, &typebool.gen);
		t = &typebool.gen;
		break;
	case TEQL:
	case TNEQ:
		t = &typebool.gen;
		if (lp & PROPARITH && rp & PROPARITH) {
			commonreal(&l, &r);
			break;
		}
		if (typeeval(l->type)->kind != TYPEPOINTER)
			e = l, l = r, r = e;
		if (typeeval(l->type)->kind != TYPEPOINTER)
			error(loc, "invalid operands to '%s' operator", tokstr[op]);
		if (nullpointer(eval(r, EVALARITH))) {
			r = exprconvert(r, r->qual, l->type);
			break;
		}
		if (nullpointer(eval(l, EVALARITH))) {
			l = exprconvert(l, l->qual, r->type);
			break;
		}
		if (typeeval(r->type)->kind != TYPEPOINTER)
			error(loc, "invalid operands to '%s' operator", tokstr[op]);
		if (typeeval(typeeval(l->type)->base)->kind == TYPEVOID)
			e = l, l = r, r = e;
		if (typeeval(typeeval(r->type)->base)->kind == TYPEVOID && typeeval(typeeval(l->type)->base)->kind != TYPEFUNC)
			r = exprconvert(r, r->qual, l->type);
		else if (!typeequal(typeeval(l->type)->base, typeeval(r->type)->base))
			error(loc, "pointer operands to '%s' operator are to different types", tokstr[op]);
		break;
	case TLESS:
	case TGREATER:
	case TLEQ:
	case TGEQ:
		t = &typebool.gen;
		if (lp & PROPREAL && rp & PROPREAL) {
			commonreal(&l, &r);
		} else if (typeeval(l->type)->kind == TYPEPOINTER && typeeval(r->type)->kind == TYPEPOINTER) {
			if (!typeequal(typeeval(l->type)->base, typeeval(r->type)->base) || typeeval(typeeval(l->type)->base)->kind == TYPEFUNC)
				error(loc, "pointer operands to '%s' operator must be to same object type", tokstr[op]);
		} else {
			error(loc, "invalid operands to '%s' operator", tokstr[op]);
		}
		break;
	case TBOR:
	case TXOR:
	case TBAND:
		t = commonreal(&l, &r);
		break;
	case TADD:
		if (!(lp & PROPARITH) || !(rp & PROPARITH))
			error(loc, "invalid operands to '+' operator");
		t = commonreal(&l, &r);
		break;
	case TSUB:
		if (lp & PROPARITH && rp & PROPARITH) {
			t = commonreal(&l, &r);
			break;
		}
		if (typeeval(l->type)->kind != TYPEPOINTER || !(rp & PROPINT) && typeeval(r->type)->kind != TYPEPOINTER)
			error(loc, "invalid operands to '-' operator");
		if (typeeval(typeeval(l->type)->base)->incomplete || !(typeeval(typeeval(l->type)->base)->prop & PROPOBJECT))
			error(loc, "pointer operand to '-' must be to complete object type");
		if (rp & PROPINT) {
			t = l->type;
			r = mkbinaryexpr(NULL, loc, TMUL, exprconvert(r, r->qual, &targ->typeulong->gen), mkconstexpr(&targ->typeulong->gen, typeeval(typeeval(t)->base)->size));
		} else {
			if (!typeequal(typeeval(l->type)->base, typeeval(r->type)->base))
				error(&tok.loc, "pointer operands to '-' are to different types");
			t = typeeval(l->type)->base;
			e = l;
			l = mkexpr(EXPRCAST, &targ->typelong->gen);
			l->base = e;
			e = r;
			r = mkexpr(EXPRCAST, &targ->typelong->gen);
			r->base = e;
			l = mkbinaryexpr(NULL, loc, TSUB, l, r);
			r = mkconstexpr(&targ->typelong->gen, typeeval(t)->size);
			op = TDIV;
			t = &targ->typelong->gen;
		}
		break;
	case TMOD:
		if (!(lp & PROPINT) || !(rp & PROPINT))
			error(loc, "operands to '%%' operator must be integer");
		t = commonreal(&l, &r);
		break;
	case TMUL:
	case TDIV:
		if (!(lp & PROPARITH) || !(rp & PROPARITH))
			error(loc, "operands to '%s' operator must be arithmetic", tokstr[op]);
		t = commonreal(&l, &r);
		break;
	case TSHL:
	case TSHR:
		if (!(lp & PROPINT) || !(rp & PROPINT))
			error(loc, "operands to '%s' operator must be integer", tokstr[op]);
		l = exprpromote(l);
		r = exprpromote(r);
		t = l->type;
		break;
	default:
		fatal("internal error: unknown binary operator %d", op);
	}
	e = mkexpr(EXPRBINARY, t);
	e->op = op;
	e->binary.l = l;
	e->binary.r = r;

	return e;
}

static struct typegen *
inttype(unsigned long long val, char *end)
{
	struct typegen *limits[] = {
		&targ->typeint->gen, &targ->typeuint->gen,
		&targ->typelong->gen, &targ->typeulong->gen,
		&typei64.gen, &typeu64.gen,
	};
	struct type *t;
	size_t i, step;

	for (i = 0; end[i]; ++i)
		end[i] = tolower(end[i]);
	step = end[i-1] == 'u' ? 2 : 1;
	for (i = end[i-1] == 'u' ? 1 : 0; i < LEN(limits); i += step) {
		t = (struct type *) limits[i];
		if (val <= 0xffffffffffffffffu >> (8 - t->size << 3) + t->basic.issigned)
			return &t->gen;
	}
	error(&tok.loc, "no suitable type for constant '%s'", tok.lit);
}

static int
isodigit(int c)
{
	return '0' <= c && c <= '8';
}

static int
unescape(char **p)
{
	int c;
	char *s = *p;

	if (*s == '\\') {
		++s;
		switch (*s) {
		case '\'':
		case '"':
		case '?':
		case '\\': c = *s;   ++s; break;
		case 'a':  c = '\a'; ++s; break;
		case 'b':  c = '\b'; ++s; break;
		case 'f':  c = '\f'; ++s; break;
		case 'n':  c = '\n'; ++s; break;
		case 'r':  c = '\r'; ++s; break;
		case 't':  c = '\t'; ++s; break;
		case 'v':  c = '\v'; ++s; break;
		case 'x':
			++s;
			assert(isxdigit(*s));
			c = 0;
			do c = c * 16 + (*s > '9' ? 10 + tolower(*s) - 'a' : *s - '0');
			while (isxdigit(*++s));
			break;
		default:
			assert(isodigit(*s));
			c = 0;
			do c = c * 8 + (*s - '0');
			while (isodigit(*++s));
		}
	} else {
		c = *s++;
	}
	*p = s;
	return c;
}

static struct expr *
generic(struct scope *s)
{
	struct expr *e, *match = NULL, *def = NULL;
	struct typegen *t, *want;
	enum typequal qual;

	next();
	expect(TLPAREN, "after '_Generic'");
	e = condexpr(s);
	expect(TCOMMA, "after generic selector expression");
	want = e->type;
	delexpr(e);
	do {
		if (consume(TELSE)) {
			if (def)
				error(&tok.loc, "multiple else expressions in generic association list");
			expect(TCOLON, "after 'else'");
			def = condexpr(s);
		} else {
			qual = QUALNONE;
			t = typename(s, &qual);
			if (!t)
				error(&tok.loc, "expected typename for generic association");
			expect(TCOLON, "after type name");
			e = condexpr(s);
			if (typeequal(t, want) && qual == QUALNONE) {
				if (match)
					error(&tok.loc, "generic selector matches multiple associations");
				match = e;
			} else {
				delexpr(e);
			}
		}
	} while (consume(TCOMMA));
	expect(TRPAREN, "after generic assocation list");
	if (!match) {
		if (!def)
			error(&tok.loc, "generic selector matches no associations and no default was specified");
		match = def;
	} else if (def) {
		delexpr(def);
	}
	return match;
}

/* 6.5 Expressions */
static struct expr *
primaryexpr(struct scope *s)
{
	struct expr *e;
	struct decl *d;
	struct typegen *t;
	struct type *type;
	char *src, *dst, *end;
	int base;

	switch (tok.kind) {
	case TIDENT:
		d = scopegetdecl(s, tok.lit, 1);
		if (!d)
			error(&tok.loc, "undeclared identifier: %s", tok.lit);
		e = mkexpr(EXPRIDENT, d->type);
		e->qual = d->qual;
		e->lvalue = d->kind == DECLOBJECT;
		e->ident.decl = d;
		next();
		break;
	case TELLIPSIS:
		next();
		e = mkexpr(EXPRBUILTIN, &typevalistptr.gen);
		e->builtin.kind = BUILTINVASTART;
		e->base = mkexpr(EXPRCOMPOUND, &typevalist.gen);
		e->base->qual = QUALMUT;
		e->base->lvalue = true;
		e->base->compound.init = NULL;
		e->base = mkunaryexpr(TBAND, e->base);
		e = mkunaryexpr(TMUL, e);
		break;
	case TSTRINGLIT:
		type = mkarraytype(&typechar.gen, QUALNONE, 0);
		e = mkexpr(EXPRSTRING, &type->gen);
		e->lvalue = true;
		e->string.size = 0;
		e->string.data = NULL;
		do {
			e->string.data = xreallocarray(e->string.data, e->string.size + strlen(tok.lit), 1);
			dst = e->string.data + e->string.size;
			src = tok.lit;
			if (*src != '"')
				fatal("wide string literal not yet implemented");
			for (++src; *src != '"'; ++dst)
				*dst = unescape(&src);
			e->string.size = dst - e->string.data;
			next();
		} while (tok.kind == TSTRINGLIT);
		type->array.length = e->string.size + 1;
		type->size = type->array.length * typechar.size;
		type->incomplete = false;
		break;
	case TCHARCONST:
		src = tok.lit;
		t = &typechar.gen;
		if (*src == 'L') {
			++src;
			t = &targ->typerune->gen;
		}
		assert(*src == '\'');
		++src;
		e = mkconstexpr(t, unescape(&src));
		if (*src != '\'')
			error(&tok.loc, "character constant contains more than one character: %c", *src);
		next();
		break;
	case TNUMBER:
		e = mkexpr(EXPRCONST, NULL);
		base = tok.lit[0] != '0' ? 10 : tolower(tok.lit[1]) == 'x' ? 16 : 8;
		if (strpbrk(tok.lit, base == 16 ? ".pP" : ".eE")) {
			/* floating constant */
			errno = 0;
			e->constant.f = strtod(tok.lit, &end);
			if (errno && errno != ERANGE)
				error(&tok.loc, "invalid floating constant '%s': %s", tok.lit, strerror(errno));
			if (!end[0])
				e->type = &typef64.gen;
			else if (tolower(end[0]) == 'f' && !end[1])
				e->type = &typef32.gen;
			else
				error(&tok.loc, "invalid floating constant suffix '%s'", end);
		} else {
			/* integer constant */
			errno = 0;
			e->constant.i = strtoull(tok.lit, &end, 0);
			if (errno)
				error(&tok.loc, "invalid integer constant '%s': %s", tok.lit, strerror(errno));
			e->type = inttype(e->constant.i, end);
		}
		next();
		break;
	case TLPAREN:
		next();
		e = expr(s);
		expect(TRPAREN, "after expression");
		break;
	case T_GENERIC:
		e = generic(s);
		break;
	default:
		error(&tok.loc, "expected primary expression");
	}

	return e;
}

/* TODO: merge with init.c:designator() */
static void
designator(struct scope *s, struct typegen *type, uint64_t *offset)
{
	struct type *t;
	char *name;
	struct member *m;
	uint64_t i;

	t = typeeval(type);

	for (;;) {
		switch (tok.kind) {
		case TLBRACK:
			if (t->kind != TYPEARRAY)
				error(&tok.loc, "index designator is only valid for array types");
			next();
			i = intconstexpr(s, false);
			expect(TRBRACK, "for index designator");
			t = typeeval(t->base);
			*offset += i * t->size;
			break;
		case TPERIOD:
			if (t->kind != TYPESTRUCT && t->kind != TYPEUNION)
				error(&tok.loc, "member designator only valid for struct/union types");
			next();
			name = expect(TIDENT, "for member designator");
			m = typemember(t, name, offset);
			if (!m)
				error(&tok.loc, "%s has no member named '%s'", t->kind == TYPEUNION ? "union" : "struct", name);
			t = typeeval(m->type);
			break;
		default:
			return;
		}
	}
}

static struct expr *
builtinfunc(struct scope *s, enum builtinkind kind)
{
	struct expr *e;
	struct typegen *t;
	struct member *m;
	char *name;
	uint64_t offset;

	switch (kind) {
	case BUILTINALLOCA:
		e = mkexpr(EXPRBUILTIN, &mkpointertype(&typevoid.gen, QUALMUT)->gen);
		e->builtin.kind = BUILTINALLOCA;
		e->base = exprconvert(condexpr(s), QUALNONE, &targ->typeulong->gen);
		break;
	case BUILTINCONSTANTP:
		e = mkconstexpr(&typebool.gen, eval(condexpr(s), EVALARITH)->kind == EXPRCONST);
		break;
	case BUILTINEXPECT:
		/* just a no-op for now */
		/* TODO: check that the expression and the expected value have type 'long' */
		e = condexpr(s);
		expect(TCOMMA, "after expression");
		delexpr(condexpr(s));
		break;
	case BUILTININFF:
		e = mkexpr(EXPRCONST, &typef64.gen);
		/* TODO: use INFINITY here when we can handle musl's math.h */
		e->constant.f = strtod("inf", NULL);
		break;
	case BUILTINNANF:
		e = condexpr(s);
		if (e->kind != EXPRSTRING || e->string.size > 0)
			error(&tok.loc, "__builtin_nanf currently only supports empty string literals");
		e = mkexpr(EXPRCONST, &typef64.gen);
		/* TODO: use NAN here when we can handle musl's math.h */
		e->constant.f = strtod("nan", NULL);
		break;
	case BUILTINOFFSETOF:
		t = typename(s, NULL);
		expect(TCOMMA, "after type name");
		name = expect(TIDENT, "after ','");
		if (typeeval(t)->kind != TYPESTRUCT && typeeval(t)->kind != TYPEUNION)
			error(&tok.loc, "type is not a struct/union type");
		offset = 0;
		m = typemember(typeeval(t), name, &offset);
		if (!m)
			error(&tok.loc, "struct/union has no member named '%s'", name);
		designator(s, m->type, &offset);
		e = mkconstexpr(&targ->typeulong->gen, offset);
		break;
	case BUILTINTYPESCOMPATIBLEP:
		t = typename(s, NULL);
		expect(TCOMMA, "after type name");
		e = mkconstexpr(&typebool.gen, typeequal(t, typename(s, NULL)));
		break;
	case BUILTINVAARG:
		e = mkexpr(EXPRBUILTIN, NULL);
		e->builtin.kind = BUILTINVAARG;
		e->base = exprconvert(condexpr(s), QUALNONE, &typevalistptr.gen);
		expect(TCOMMA, "after va_list");
		e->type = typename(s, &e->qual);
		break;
	default:
		fatal("internal error; unknown builtin");
	}
	return e;
}

static struct expr *
mkincdecexpr(struct scope *s, enum tokenkind op, struct expr *base, bool post)
{
	struct expr *e, *l, *r, *tmp1, *tmp2;
	struct decl *d;

	if (!base->lvalue)
		error(&tok.loc, "operand of '%s' operator must be an lvalue", tokstr[op]);
	e = mkunaryexpr(TBAND, base);
	d = scopegetdecl(s, mangleuop(op, e->type), true);
	if (d) {
		/* rewrite `E OP` as `T1 = &E, T2 = *T1, OP(T1), T2`
		   and `OP E` as `OP(&E)` */
		if (post) {
			l = exprtemp(&tmp1, e);
			l->next = exprtemp(&tmp2, mkunaryexpr(TMUL, tmp1));
			r = l->next;
			r->next = mkoverloadexpr(d, tmp1, NULL);
			r = r->next;
			r->next = tmp2;
			e = mkexpr(EXPRCOMMA, tmp2->type);
			e->base = l;
			e->qual = tmp2->qual;
		} else {
			e = mkoverloadexpr(d, e, NULL);
		}
		return e;
	}
	if (!(base->qual & QUALMUT))
		error(&tok.loc, "operand of '%s' operator is not mutable", tokstr[op]);
	if (!(typeeval(base->type)->prop & PROPSCALAR))
		error(&tok.loc, "operand of '%s' is not overloaded", tokstr[op]);
	e = mkexpr(EXPRINCDEC, base->type);
	e->op = op;
	e->base = base;
	e->qual = base->qual;
	e->incdec.post = post;
	return e;
}

static struct expr *
postfixexpr(struct scope *s, struct expr *r)
{
	struct expr *e, *arr, *idx, *tmp, **end;
	struct type *t;
	struct param *p;
	struct decl *d;
	struct member *m;
	uint64_t offset;
	enum typequal tq;
	char *name;
	bool lvalue;

	if (!r)
		r = primaryexpr(s);
	for (;;) {
		switch (tok.kind) {
		case TLBRACK:  /* subscript */
			next();
			if (typeeval(r->type)->kind == TYPEPARAM) {
				t = mkapplytype(r->type, 0, NULL);
				offset = 0;
				do {
					t->apply.vars = xreallocarray(t->apply.vars, sizeof(*t->apply.vars), offset+1);
					t->apply.vars[offset] = typename(s, NULL);
					offset++;
				} while (consume(TCOMMA));
				t->apply.nvars = offset;
				expect(TRBRACK, "after type function call");
				e = r;
				e->type = t;
			} else {
				arr = r;
				idx = expr(s);
				t = typeeval(arr->type);
				if (t->kind == TYPEPOINTER)
					t = typeeval(t->base);
				else
					arr = mkunaryexpr(TBAND, arr);
				if (t->kind != TYPEARRAY)
					error(&tok.loc, "cannot index non-array type");
				if (typeeval(t->base)->incomplete)
					error(&tok.loc, "cannot index array of incomplete type");
				if (!(typeeval(idx->type)->prop & PROPINT))
					error(&tok.loc, "index is not an integer type");
				e = mkbinaryexpr(NULL, &tok.loc, TMUL, idx, mkconstexpr(&targ->typeulong->gen, typeeval(t->base)->size));
				/* XXX where to take qualifiers from? */
				t = mkpointertype(t->base, typeeval(arr->type)->qual);
				arr->type = &targ->typeulong->gen;
				e = mkbinaryexpr(NULL, &tok.loc, TADD, arr, e);
				e->type = &t->gen;
				e = mkunaryexpr(TMUL, e);
				expect(TRBRACK, "after array index");
			}
			break;
		case TLPAREN:  /* function call */
			next();
			if (r->kind == EXPRIDENT && r->ident.decl->kind == DECLBUILTIN) {
				e = builtinfunc(s, r->ident.decl->builtin);
				expect(TRPAREN, "after builtin parameters");
				break;
			}
			if (typeeval(r->type)->kind == TYPEFUNC)
				r = mkunaryexpr(TBAND, r);
			if (typeeval(r->type)->kind != TYPEPOINTER || typeeval(typeeval(r->type)->base)->kind != TYPEFUNC)
				error(&tok.loc, "called object is not a function");
			t = typeeval(typeeval(r->type)->base);
			e = mkexpr(EXPRCALL, t->base);
			e->base = r;
			e->call.args = NULL;
			e->call.nargs = 0;
			p = t->func.params;
			end = &e->call.args;
			while (tok.kind != TRPAREN) {
				if (e->call.args)
					expect(TCOMMA, "or ')' after function call argument");
				if (!p && !t->func.isvararg)
					error(&tok.loc, "too many arguments for function call");
				*end = condexpr(s);
				if (t->func.isvararg && !p)
					*end = exprpromote(*end);
				else
					*end = exprconvert(*end, p->qual, p->type);
				end = &(*end)->next;
				++e->call.nargs;
				if (p)
					p = p->next;
			}
			if (p && !t->func.isvararg)
				error(&tok.loc, "not enough arguments for function call");
			next();
			break;
		case TARROW:
			next();
			if (consume(TLBRACK)) {
				t = typeeval(typename(s, NULL));
				if (!t)
					error(&tok.loc, "expected type on cast expression");
				expect(TRBRACK, "to close '[' in cast");
				if (t->prop & PROPSCALAR) {
					e = mkexpr(EXPRCAST, &t->gen);
					e->base = r;
				} else {
					/* XXX HACK FIXME */
					e->type = &t->gen;
				}
				break;
			} else if (consume(TAUTO)) {
				if (tok.kind != TIDENT)
					error(&tok.loc, "expected identifier");
				name = tok.lit;
				next();
				expect(TLPAREN, "on auto expression");
				s = mkscope(s);
				d = mkdecl(DECLOBJECT, r->type, QUALNONE, LINKNONE);
				scopeputdecl(s, name, d);
				funcinit(s->func, d, NULL, name);
				e = mkexpr(EXPRIDENT, r->type);
				e->qual = QUALMUT;
				e->lvalue = true;
				e->ident.decl = d;
				r = mkassignexpr(e, r);
				e = expr(s);
				r->next = e;
				e = mkexpr(EXPRCOMMA, e->type);
				e->base = r;
				expect(TRPAREN, "after auto expression");
				s = delscope(s);
				break;
			}
			error(&tok.loc, "'->' not implemented yet");
			break;
		case TPERIOD:
			next();
			if (tok.kind != TIDENT)
				error(&tok.loc, "expected identifier after '.' operator");
			lvalue = typeeval(r->type)->kind == TYPEPOINTER || r->lvalue;
			if (typeeval(r->type)->kind != TYPEPOINTER)
				r = mkunaryexpr(TBAND, r);
			t = typeeval(typeeval(r->type)->base);
			tq = typeeval(r->type)->qual;
			if (t->kind != TYPESTRUCT && t->kind != TYPEUNION)
				error(&tok.loc, "'.' operator must be applied to a struct or union");
			tmp = r;
			r = mkexpr(EXPRCAST, &targ->typeulong->gen);
			r->base = tmp;
			offset = 0;
			m = typemember(t, tok.lit, &offset);
			if (!m)
				error(&tok.loc, "struct/union has no member named '%s'", tok.lit);
			r = mkbinaryexpr(NULL, &tok.loc, TADD, r, mkconstexpr(&targ->typeulong->gen, offset));
			tmp = r;
			r = mkexpr(EXPRCAST, &mkpointertype(m->type, tq | m->qual)->gen);
			r->base = tmp;
			r = mkunaryexpr(TMUL, r);
			r->lvalue = lvalue;
			if (m->bits.before || m->bits.after) {
				e = mkexpr(EXPRBITFIELD, r->type);
				e->lvalue = lvalue;
				e->base = r;
				e->qual = r->qual;
				e->bitfield.bits = m->bits;
			} else {
				e = r;
			}
			next();
			break;
		case TINC:
		case TDEC:
			e = mkincdecexpr(s, tok.kind, r, true);
			next();
			break;
		default:
			return r;
		}
		r = e;
	}
}

static struct expr *castexpr(struct scope *);

static struct expr *
unaryexpr(struct scope *s)
{
	enum tokenkind op;
	struct expr *e, *l;
	struct type *t;
	struct decl *d;
	char *name;

	op = tok.kind;
	switch (op) {
	case TINC:
	case TDEC:
		next();
		l = unaryexpr(s);
		e = mkincdecexpr(s, op, l, false);
		break;
	case TBAND:
	case TMUL:
		next();
		return mkunaryexpr(op, castexpr(s));
	case TADD:
		next();
		e = castexpr(s);
		d = scopegetdecl(s, mangleuop(TADD, e->type), true);
		if (d) {
			e = mkoverloadexpr(d, e, NULL);
			break;
		}
		if (!(typeeval(e->type)->prop & PROPARITH))
			error(&tok.loc, "operand of unary '+' operator must have arithmetic type");
		if (typeeval(e->type)->prop & PROPINT)
			e = exprpromote(e);
		break;
	case TSUB:
		next();
		e = castexpr(s);
		d = scopegetdecl(s, mangleuop(TSUB, e->type), true);
		if (d) {
			e = mkoverloadexpr(d, e, NULL);
			break;
		}
		if (!(typeeval(e->type)->prop & PROPARITH))
			error(&tok.loc, "operand of unary '-' operator must have arithmetic type");
		if (typeeval(e->type)->prop & PROPINT)
			e = exprpromote(e);
		e = mkbinaryexpr(s, &tok.loc, TSUB, mkconstexpr(&targ->typeint->gen, 0), e);
		break;
	case TBNOT:
		next();
		e = castexpr(s);
		d = scopegetdecl(s, mangleuop(TBNOT, e->type), true);
		if (d) {
			e = mkoverloadexpr(d, e, NULL);
			break;
		}
		if (!(typeeval(e->type)->prop & PROPINT))
			error(&tok.loc, "operand of '~' operator must have integer type");
		e = exprpromote(e);
		e = mkbinaryexpr(s, &tok.loc, TXOR, e, mkconstexpr(e->type, -1));
		break;
	case TLNOT:
		next();
		e = castexpr(s);
		if (!(typeeval(e->type)->prop & PROPSCALAR))
			error(&tok.loc, "operator '!' must have scalar operand");
		e = mkbinaryexpr(s, &tok.loc, TEQL, e, mkconstexpr(e->type, 0));
		break;
	case TGOTO:
		next();
		name = expect(TIDENT, "after 'goto'");
		e = mkexpr(EXPRJUMP, &typenoreturn.gen);
		e->label = funcgoto(s->func, name)->label;
		break;
	case TCONTINUE:
		if (!s->continuelabel)
			error(&tok.loc, "'continue' must be in loop or switch");
		next();
		e = mkexpr(EXPRJUMP, &typenoreturn.gen);
		e->label = s->continuelabel;
		if (s->switchcond) {
			l = condexpr(s);
			l = mkassignexpr(s->switchcond, l);
			l->next = e;
			e = mkexpr(EXPRCOMMA, &typenoreturn.gen);
			e->base = l;
			e->qual = QUALNONE;
		}
		break;
	case TBREAK:
		if (!s->breaklabel)
			error(&tok.loc, "'break' must be in loop or switch");
		next();
		e = mkexpr(EXPRJUMP, &typenoreturn.gen);
		e->label = s->breaklabel;
		break;
	case TRETURN:
		next();
		t = functype(s->func);
		l = NULL;
		if (t->base != &typevoid.gen)
			l = exprconvert(expr(s), t->qual, t->base);
		e = mkexpr(EXPRRET, &typenoreturn.gen);
		e->base = l;
		break;
	case TSIZEOF:
	case TALIGNOF:
		next();
		if (consume(TLPAREN)) {
			t = NULL;
			e = expr(s);
			expect(TRPAREN, "after expression");
		} else {
			expect(TLBRACK, "before type");
			t = typeeval(typename(s, NULL));
			expect(TRBRACK, "after type");
		}
		if (!t) {
			if (e->kind == EXPRBITFIELD)
				error(&tok.loc, "%s operator applied to bitfield expression", tokstr[op]);
			t = typeeval(e->type);
		}
		if (t->incomplete)
			error(&tok.loc, "%s operator applied to incomplete type", tokstr[op]);
		if (t->kind == TYPEFUNC)
			error(&tok.loc, "%s operator applied to function type", tokstr[op]);
		e = mkconstexpr(&targ->typeulong->gen, op == TSIZEOF ? t->size : t->align);
		e = postfixexpr(s, e);
		break;
	default:
		e = postfixexpr(s, NULL);
	}

	return e;
}

static struct expr *
castexpr(struct scope *s)
{
	struct typegen *t;
	enum typequal tq;
	struct expr *r, *e, **end;

	end = &r;
	if (consume(TLPAREN)) {
		e = expr(s);
		expect(TRPAREN, "after expression to match '('");
		*end = postfixexpr(s, e);
		return r;
	} else if (consume(TLBRACK)) {
		tq = QUALNONE;
		t = typename(s, &tq);
		if (!t)
			error(&tok.loc, "expected type after '[' in compound literal");
		expect(TRBRACK, "after type name in compound literal");
		if (tok.kind != TLPAREN)
			error(&tok.loc, "expected '(' after type in compound literal");
		e = mkexpr(EXPRCOMPOUND, t);
		e->qual = tq;
		e->lvalue = true;
		e->compound.init = parseinit(s, t);
		*end = postfixexpr(s, e);
		return r;
	}
	*end = unaryexpr(s);

	return r;
}

static int
precedence(enum tokenkind t)
{
	switch (t) {
	case TLOR:     return 0;
	case TLAND:    return 1;
	case TBOR:     return 2;
	case TXOR:     return 3;
	case TBAND:    return 4;
	case TEQL:
	case TNEQ:     return 5;
	case TLESS:
	case TGREATER:
	case TLEQ:
	case TGEQ:     return 6;
	case TSHL:
	case TSHR:     return 7;
	case TADD:
	case TSUB:     return 8;
	case TMUL:
	case TDIV:
	case TMOD:     return 9;
	}
	return -1;
}

static struct expr *
binaryexpr(struct scope *s, struct expr *l, int i)
{
	struct expr *r;
	struct location loc;
	enum tokenkind op;
	int j, k;

	if (!l)
		l = castexpr(s);
	while ((j = precedence(tok.kind)) >= i) {
		op = tok.kind;
		loc = tok.loc;
		next();
		r = castexpr(s);
		while ((k = precedence(tok.kind)) > j)
			r = binaryexpr(s, r, k);
		l = mkbinaryexpr(s, &loc, op, l, r);
	}
	return l;
}

static struct expr *
elseexpr(struct scope *s)
{
	struct expr *e, *l, *tmp;

	l = binaryexpr(s, NULL, 0);
	if (!consume(TELSE))
		return l;
	if (!(typeeval(l->type)->prop & PROPSCALAR))
		error(&tok.loc, "left operand of 'else' operator must be scalar");
	e = mkexpr(EXPRCOND, NULL);
	e->base = exprconvert(exprtemp(&tmp, l), l->qual, &typebool.gen);
	e->cond.t = tmp;
	e->cond.f = elseexpr(s);
	condunify(e);

	return e;
}

struct expr *
constexpr(struct scope *s)
{
	return eval(condexpr(s), EVALARITH);
}

uint64_t
intconstexpr(struct scope *s, bool allowneg)
{
	struct expr *e;

	e = constexpr(s);
	if (e->kind != EXPRCONST || !(typeeval(e->type)->prop & PROPINT))
		error(&tok.loc, "not an integer constant expression");
	if (!allowneg && typeeval(e->type)->basic.issigned && e->constant.i > INT64_MAX)
		error(&tok.loc, "integer constant expression cannot be negative");
	return e->constant.i;
}

struct expr *
mkassignexpr(struct expr *l, struct expr *r)
{
	struct expr *e;

	e = mkexpr(EXPRASSIGN, l->type);
	e->assign.l = l;
	e->assign.r = exprconvert(r, r->qual, l->type);
	return e;
}

static struct expr *
assignexpr(struct scope *s)
{
	struct expr *e, *l, *r, *tmp, *bit;
	enum tokenkind op;

	l = elseexpr(s);
	if (l->kind == EXPRBINARY || l->kind == EXPRCOMMA || l->kind == EXPRCAST)
		return l;
	switch (tok.kind) {
	case TASSIGN:     op = TNONE; break;
	case TMULASSIGN:  op = TMUL;  break;
	case TDIVASSIGN:  op = TDIV;  break;
	case TMODASSIGN:  op = TMOD;  break;
	case TADDASSIGN:  op = TADD;  break;
	case TSUBASSIGN:  op = TSUB;  break;
	case TSHLASSIGN:  op = TSHL;  break;
	case TSHRASSIGN:  op = TSHR;  break;
	case TBANDASSIGN: op = TBAND; break;
	case TXORASSIGN:  op = TXOR;  break;
	case TBORASSIGN:  op = TBOR;  break;
	default:
		return l;
	}
	if (!l->lvalue)
		error(&tok.loc, "left side of assignment expression is not an lvalue");
	if (!(l->qual & QUALMUT))
		error(&tok.loc, "left side of assignment is not mutable");
	next();
	r = condexpr(s);
	if (!op)
		return mkassignexpr(l, r);
	/* rewrite `E1 OP= E2` as `T = &E1, *T = *T OP E2`, where T is a temporary slot */
	if (l->kind == EXPRBITFIELD) {
		bit = l;
		l = l->base;
	} else {
		bit = NULL;
	}
	e = exprtemp(&tmp, mkunaryexpr(TBAND, l));
	l = mkunaryexpr(TMUL, tmp);
	if (bit) {
		bit->base = l;
		l = bit;
	}
	r = mkbinaryexpr(s, &tok.loc, op, l, r);
	e->next = mkassignexpr(l, r);
	l = mkexpr(EXPRCOMMA, l->type);
	l->base = e;
	l->qual = e->qual;
	return l;
}

struct expr *
condexpr(struct scope *s)
{
	struct expr *r, *e;

	if (!consume(TIF))
		return assignexpr(s);
	expect(TLPAREN, "after 'if'");
	r = expr(s);
	expect(TRPAREN, "after if's condition");
	e = mkexpr(EXPRCOND, NULL);
	e->base = exprconvert(r, r->qual, &typebool.gen);
	e->cond.t = binaryexpr(s, NULL, 0);
	expect(TELSE, "in conditional expression");
	e->cond.f = condexpr(s);
	condunify(e);

	return e;
}

struct expr *
exprtemp(struct expr **tmp, struct expr *e)
{
	*tmp = mkexpr(EXPRTEMP, e->type);
	(*tmp)->lvalue = true;
	(*tmp)->temp = NULL;
	e = mkassignexpr(*tmp, e);

	return e;
}

struct expr *
expr(struct scope *s)
{
	struct expr *r, *e, **end;

	end = &r;
	for (;;) {
		e = condexpr(s);
		*end = e;
		end = &e->next;
		if (tok.kind != TCOMMA)
			break;
		next();
	}
	if (!r->next)
		return r;
	e = mkexpr(EXPRCOMMA, e->type);
	e->base = r;
	e->qual = r->qual;

	return e;
}

struct expr *
exprconvert(struct expr *e, enum typequal qt, struct typegen *t)
{
	struct expr *cast;

	if ((e->qual & QUALNOCOPY) == (qt & QUALNOCOPY) && typeequal(e->type, t))
		return e;
	if (nullpointer(e) && typeeval(t)->kind == TYPEPOINTER)
		return e;
	if (!typecast(e->type, t))
		error(&tok.loc, "illegal implicit cast");
	cast = mkexpr(EXPRCAST, t);
	cast->base = e;
	cast->qual = qt;

	return cast;
}
