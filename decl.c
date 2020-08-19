#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "cc.h"

static struct list tentativedefns = {&tentativedefns, &tentativedefns};

struct qualtype {
	struct type *type;
	enum typequal qual;
};

enum storageclass {
	SCNONE,

	SCTYPEDEF     = 1<<1,
	SCPUBLIC      = 1<<2,
	SCSTATIC      = 1<<3,
	SCAUTO        = 1<<4,
	SCREGISTER    = 1<<5,
};

enum funcspec {
	FUNCNONE,

	FUNCINLINE   = 1<<1,
	FUNCNORETURN = 1<<2,
};

struct structbuilder {
	struct type *type;
	struct member **last;
	unsigned bits;  /* number of bits remaining in the last byte */
};

struct declbuilder {
	struct list list;
	char *name;
	uint64_t width;
};

struct decl *
mkdecl(enum declkind k, struct type *t, enum typequal tq, enum linkage linkage)
{
	struct decl *d;

	d = xmalloc(sizeof(*d));
	memset(d, 0, sizeof(*d));
	d->kind = k;
	d->linkage = linkage;
	d->type = t;
	d->qual = tq;

	return d;
}

/* 6.7.1 Storage-class specifiers */
static int
storageclass(enum storageclass *sc)
{
	enum storageclass new;

	switch (tok.kind) {
	case TTYPEDEF:  new = SCTYPEDEF;     break;
	case TPUB:      new = SCPUBLIC;      break;
	case TSTATIC:   new = SCSTATIC;      break;
	case TAUTO:     new = SCAUTO;        break;
	case TREGISTER: new = SCREGISTER;    break;
	default: return 0;
	}
	if (!sc)
		error(&tok.loc, "storage class not allowed in this declaration");
	if (new && *sc)
		error(&tok.loc, "invalid combination of storage class specifiers");
	*sc |= new;
	next();

	return 1;
}

/* 6.7.3 Type qualifiers */
static int
typequal(enum typequal *tq)
{
	switch (tok.kind) {
	case TMUT:      *tq |= QUALMUT;    break;
	case TVOLATILE: *tq |= QUALVOLATILE; break;
	case TRESTRICT: *tq |= QUALRESTRICT; break;
	default: return 0;
	}
	next();

	return 1;
}

/* 6.7.4 Function specifiers */
static int
funcspec(enum funcspec *fs)
{
	enum funcspec new;

	switch (tok.kind) {
	case TINLINE:   new = FUNCINLINE;   break;
	case TNORETURN: new = FUNCNORETURN; break;
	default: return 0;
	}
	if (!fs)
		error(&tok.loc, "function specifier not allowed in this declaration");
	*fs |= new;
	next();

	return 1;
}

static void structdecl(struct scope *, struct structbuilder *);

static struct type *
tagspec(struct scope *s)
{
	struct type *t;
	char *tag, *name;
	enum typekind kind;
	struct decl *d;
	struct expr *e;
	struct structbuilder b;
	uint64_t i;
	bool large;

	switch (tok.kind) {
	case TSTRUCT: kind = TYPESTRUCT; break;
	case TUNION:  kind = TYPEUNION;  break;
	case TENUM:   kind = TYPEENUM;   break;
	default: fatal("internal error: unknown tag kind");
	}
	next();
	if (tok.kind == TLPAREN) {
		tag = NULL;
		t = NULL;
	} else {
		tag = expect(TIDENT, "or '(' after struct/union");
		t = scopegettag(s, tag, false);
		if (s->parent && !t && tok.kind != TLPAREN && (kind == TYPEENUM || tok.kind != TSEMICOLON))
			t = scopegettag(s->parent, tag, true);
	}
	if (t) {
		if (t->kind != kind)
			error(&tok.loc, "redeclaration of tag '%s' with different kind", tag);
	} else {
		if (kind == TYPEENUM) {
			t = xmalloc(sizeof(*t));
			*t = *targ->typeuint;
			t->kind = kind;
		} else {
			t = mktype(kind, PROPOBJECT);
			if (kind == TYPESTRUCT)
				t->prop |= PROPAGGR;
			t->repr = &i64; // XXX
			t->size = 0;
			t->align = 0;
			t->structunion.tag = tag;
			t->structunion.members = NULL;
		}
		t->incomplete = true;
		if (tag)
			scopeputtag(s, tag, t);
	}
	if (tok.kind != TLPAREN)
		return t;
	if (!t->incomplete)
		error(&tok.loc, "redefinition of tag '%s'", tag);
	next();
	switch (t->kind) {
	case TYPESTRUCT:
	case TYPEUNION:
		b.type = t;
		b.last = &t->structunion.members;
		b.bits = 0;
		do structdecl(s, &b);
		while (tok.kind != TRPAREN);
		next();
		t->size = ALIGNUP(t->size, t->align);
		t->incomplete = false;
		break;
	case TYPEENUM:
		large = false;
		for (i = 0; tok.kind == TIDENT; ++i) {
			name = tok.lit;
			next();
			if (consume(TASSIGN)) {
				e = constexpr(s);
				if (e->kind != EXPRCONST || !(e->type->prop & PROPINT))
					error(&tok.loc, "expected integer constant expression");
				i = e->constant.i;
				if (e->type->basic.issigned && i >= 1ull << 63) {
					if (i < -1ull << 31)
						goto invalid;
					t->basic.issigned = true;
				} else if (i >= 1ull << 32) {
					goto invalid;
				}
			} else if (i == 1ull << 32) {
			invalid:
				error(&tok.loc, "enumerator '%s' value cannot be represented as 'int'", name);
			}
			d = mkdecl(DECLCONST, targ->typeint, QUALNONE, LINKNONE);
			d->value = mkintconst(t->repr, i);
			if (i >= 1ull << 31 && i < 1ull << 63) {
				large = true;
				d->type = targ->typeuint;
			}
			if (large && t->basic.issigned)
				error(&tok.loc, "neither 'int' nor 'unsigned' can represent all enumerator values");
			scopeputdecl(s, name, d);
			if (!consume(TCOMMA))
				break;
		}
		expect(TRPAREN, "to close enum specifier");
		t->incomplete = false;
	}

	return t;
}

/* 6.7 Declarations */
static struct param **parameters(struct scope *, struct param **);

static struct qualtype
decltype(struct scope *s, int *align)
{
	struct type *base, **t, *other, *type;
	struct expr *e;
	struct param **p;
	enum typequal *tq, qual = QUALNONE;
	uint64_t i;

	base = NULL;
	t = &base;
	tq = &qual;
	if (align)
		*align = 0;
	for (;;) {
		if (typequal(tq))
			continue;
		switch (tok.kind) {
		/* 6.7.2 Type specifiers */
		case TVOID:
			*t = &typevoid;
			next();
			goto done;
		case TSTRUCT:
		case TUNION:
		case TENUM:
			*t = tagspec(s);
			goto done;
		case TIDENT:
			*t = scopegettag(s, tok.lit, 1);
			if (!*t) {
				if (!importing)
					error(&tok.loc, "type '%s' does not exist", tok.lit);
				*t = mktype(TYPENONE, PROPNONE);
				(*t)->incomplete = true;
				scopeputtag(s, tok.lit, *t);
			}
			next();
			goto done;
		case TTYPEOF:
			next();
			expect(TLPAREN, "after 'typeof'");
			e = expr(s);
			if (e->decayed)
				e = e->base;
			*t = e->type;
			*tq |= e->qual;
			delexpr(e);
			expect(TRPAREN, "to close 'typeof'");
			goto done;
		case TMUL:
			next();
			*t = mkpointertype(NULL, QUALNONE);
			tq = &(*t)->qual;
			t = &(*t)->base;
			break;
		case TLBRACK:
			next();
			if (tok.kind == TRBRACK) {
				i = 0;
				next();
			} else {
				e = eval(condexpr(s), EVALARITH);
				if (e->kind != EXPRCONST || !(e->type->prop & PROPINT))
					error(&tok.loc, "VLAs are not yet supported");
				i = e->constant.i;
				if (e->type->basic.issigned && i > INT64_MAX)
					error(&tok.loc, "array length must be non-negative");
				delexpr(e);
				expect(TRBRACK, "after array length");
			}
			*t = mkarraytype(NULL, QUALNONE, i);
			tq = &(*t)->qual;
			t = &(*t)->base;
			break;
		case TLPAREN:
			next();
			*t = mktype(TYPEFUNC, PROPDERIVED);
			(*t)->qual = QUALNONE;
			(*t)->func.isvararg = false;
			(*t)->func.isnoreturn = false;
			(*t)->func.params = NULL;
			p = &(*t)->func.params;
			if (tok.kind != TRPAREN) {
				for (;;) {
					if (consume(TELLIPSIS)) {
						(*t)->func.isvararg = true;
						break;
					}
					p = parameters(s, p);
					if (!consume(TCOMMA))
						break;
				}
				if ((*t)->func.params->type->kind == TYPEVOID && !(*t)->func.params->next)
					(*t)->func.params = NULL;
			}
			expect(TRPAREN, "to close function declarator");
			(*t)->func.paraminfo = true;
			tq = &(*t)->qual;
			t = &(*t)->base;
			break;

		/* 6.7.5 Alignment specifier */
		case TALIGNAS:
			if (!align)
				error(&tok.loc, "alignment specifier not allowed in this declaration");
			next();
			expect(TLPAREN, "after 'alignas'");
			other = typename(s, NULL);
			if (other) {
				*align = other->align;
			} else {
				i = intconstexpr(s, false);
				if (!i || i & (i - 1) || i > 16)
					error(&tok.loc, "invalid alignment: %d", i);
				*align = (int)i;
			}
			expect(TRPAREN, "to close 'alignas' specifier");
			break;

		default:
			goto done;
		}
	}
done:
	if (!*t && base)
		error(&tok.loc, "expecting type");
	if (base && qual && base->kind == TYPEARRAY) {
		base = mkarraytype(base->base, base->qual | qual, base->array.length);
		qual = QUALNONE;
	}
	for (type = base; type && type->prop & PROPDERIVED; type = type->base) {
		if (type->prop & PROPDERIVED && !type->base)
			error(&tok.loc, "expected type");
		switch (type->kind) {
		case TYPEARRAY:
			if (type->base->incomplete)
				error(&tok.loc, "array element has incomplete type");
			type->align = 0;
			type->size = type->array.length;
			for (other = type->base; other; other = other->base) {
				if (other->kind == TYPEARRAY) {
					type->size *= other->array.length; // XXX: overflow?
				} else {
					type->size *= other->size;
					type->align = other->align;
					break;
				}
			}
			break;
		case TYPEFUNC:
			if (type->base->kind == TYPEFUNC)
				error(&tok.loc, "function declarator specifies function return type");
			if (type->base->kind == TYPEARRAY)
				error(&tok.loc, "function declarator specifies array return type");
			break;
		case TYPEPOINTER:
			break;
		default:
			goto finish;
		}
	}
finish:
	return (struct qualtype){base, qual};
}

static struct declbuilder *
mkdeclbuilder(char *name, uint64_t width)
{
	struct declbuilder *db;

	db = xmalloc(sizeof(*db));
	memset(db, 0, sizeof(*db));
	db->name = name;
	db->width = width;

	return db;
}

static struct qualtype
declaration(struct scope *s, enum storageclass *sc, enum funcspec *fs, struct list *db, int *align, bool needsc)
{
	struct qualtype qt;
	struct type *t;
	struct param **p;
	struct declbuilder *cur;
	enum tokenkind overload = TNONE;
	enum typequal tq;
	char *name;
	int64_t width;

	t = NULL;
	if (sc)
		*sc = SCNONE;
	if (fs)
		*fs = FUNCNONE;
	while (funcspec(fs) || storageclass(sc))
		{}
	if (sc && !*sc && needsc)
		return (struct qualtype){NULL, QUALNONE};

	switch (tok.kind) {
	case TIDENT:
		do {
			if (tok.kind != TIDENT)
				error(&tok.loc, "expected identifier");
			name = tok.lit;
			next();
			width = -1;
			if (consume(TCOLON)) {
				typequal(&tq);
				width = intconstexpr(s, true);
				if (width < 0) {
					width = -width;
					t = targ->typeint;
				} else {
					t = targ->typeuint;
				}
				if (t == targ->typeint && width >= targ->typeint->size * 8)
					t = targ->typelong;
				else if (t == targ->typeuint && width > targ->typeuint->size * 8)
					t = targ->typeulong;
				qt.type = t;
				qt.qual = tq;
			}
			cur = mkdeclbuilder(name, width);
			listinsert(db->prev, &cur->list);
			if (width != -1)
				return qt;
		} while (consume(TCOMMA));
		break;
	case TPERIOD:
		next();
		switch(tok.kind) {
		case TINC:
		case TDEC:
		case TBAND:
		case TMUL:
		case TADD:
		case TSUB:
		case TBNOT:
		case TLNOT:
		case TDIV:
		case TMOD:
		case TSHL:
		case TSHR:
		case TLESS:
		case TGREATER:
		case TLEQ:
		case TGEQ:
		case TEQL:
		case TNEQ:
		case TXOR:
		case TBOR:
		case TLAND:
		case TLOR:
		case TELLIPSIS:
		case TASSIGN:
			overload = tok.kind;
			break;
		default:
			error(&tok.loc, "unsupported overload");
		}
		next();
		break;
	case TSTRUCT:
	case TUNION:
	case TENUM:
		break;
	default:
		return (struct qualtype){NULL, QUALNONE};
	}
	if (fs && tok.kind == TLPAREN) {
		next();
		t = mktype(TYPEFUNC, PROPDERIVED);
		t->qual = QUALNONE;
		t->func.isvararg = false;
		t->func.isnoreturn = false;
		t->func.params = NULL;
		p = &t->func.params;
		if (tok.kind != TRPAREN) {
			for (;;) {
				if (consume(TELLIPSIS)) {
					t->func.isvararg = true;
					break;
				}
				p = parameters(s, p);
				if (!consume(TCOMMA))
					break;
			}
		}
		expect(TRPAREN, "to close function declarator");
		t->func.paraminfo = t->func.params || tok.kind == TLBRACE;
		if (overload) {
			cur = mkdeclbuilder(manglegen(overload, t), -1);
			listinsert(db->prev, &cur->list);
		}
	} else if (overload)
		error(&tok.loc, "overload must be function");
	qt = decltype(s, align);
	if (t) {
		if (!qt.type)
			error(&tok.loc, "function has no return type");
		if (qt.type->kind == TYPEFUNC)
			error(&tok.loc, "function returns function");
		if (qt.type->kind == TYPEARRAY)
			error(&tok.loc, "function returns array");
		t->base = qt.type;
		qt.type = t;
	}
	return qt;
}

static struct type *
adjust(struct type *t)
{
	switch (t->kind) {
	case TYPEARRAY:
		t = mkpointertype(t->base, t->qual);
		break;
	case TYPEFUNC:
		t = mkpointertype(t, QUALNONE);
		break;
	}

	return t;
}

static struct param **
parameters(struct scope *s, struct param **p)
{
	struct list *names, db = {&db, &db};
	struct qualtype t;
	enum storageclass sc;
	struct declbuilder *decl;

	t = declaration(s, &sc, NULL, &db, NULL, false);
	if (sc && sc != SCREGISTER)
		error(&tok.loc, "parameter declaration has invalid storage-class specifier");
	if (!t.type)
		error(&tok.loc, "invalid parameter declaration (no name?)");

	for (names = db.next; names != &db; names = names->next) {
		decl = listelement(names, struct declbuilder, list);
		if (decl->width != -1)
			error(&tok.loc, "invalid bit-field parameter");
		*p = mkparam(decl->name, adjust(t.type), t.qual);
		p = &(*p)->next;
	}

	return p;
}

static void
addmember(struct structbuilder *b, struct qualtype mt, char *name, int align, uint64_t width)
{
	struct type *t = b->type;
	struct member *m;
	size_t end;

	// XXX: check incomplete type, except for flexible array member
	if (mt.type->kind == TYPEFUNC)
		error(&tok.loc, "struct member '%s' has function type", name);
	assert(mt.type->align > 0);
	if (strcmp(name, "_") || width == -1) {
		m = xmalloc(sizeof(*m));
		m->type = mt.type;
		m->qual = mt.qual;
		m->name = name;
		m->next = NULL;
		*b->last = m;
		b->last = &m->next;
	} else {
		m = NULL;
	}
	if (width == -1) {
		m->bits.before = 0;
		m->bits.after = 0;
		if (align < mt.type->align)
			align = mt.type->align;
		t->size = ALIGNUP(t->size, align);
		if (t->kind == TYPESTRUCT) {
			m->offset = t->size;
			t->size += mt.type->size;
		} else {
			m->offset = 0;
			if (t->size < mt.type->size)
				t->size = mt.type->size;
		}
		b->bits = 0;
	} else {  /* bit-field */
		if (!width && strcmp(name, "_"))
			error(&tok.loc, "bit-field with zero width must be unnamed");
		if (width > mt.type->size * 8)
			error(&tok.loc, "bit-field exceeds width of underlying type");
		align = mt.type->align;
		if (t->kind == TYPESTRUCT) {
			/* calculate end of the storage-unit for this bit-field */
			end = ALIGNUP(t->size, mt.type->size);
			if (!width || width > (end - t->size) * 8 + b->bits) {
				/* no room, allocate a new storage-unit */
				t->size = end;
				b->bits = 0;
			}
			if (m) {
				m->offset = ALIGNDOWN(t->size - !!b->bits, mt.type->size);
				m->bits.before = (t->size - m->offset) * 8 - b->bits;
				m->bits.after = mt.type->size * 8 - width - m->bits.before;
			}
			t->size += (width - b->bits + 7) / 8;
			b->bits = (b->bits - width) % 8;
		} else if (m) {
			m->offset = 0;
			m->bits.before = 0;
			m->bits.after = mt.type->size * 8 - width;
			if (t->size < mt.type->size)
				t->size = mt.type->size;
		}
	}
	if (m && t->align < align)
		t->align = align;
}

static void
structdecl(struct scope *s, struct structbuilder *b)
{
	struct qualtype qt;
	struct list *names, db = {&db, &db};
	struct declbuilder *decl;
	int align;

	qt = declaration(s, NULL, NULL, &db, &align, false);
	if (!qt.type)
		error(&tok.loc, "invalid struct member declaration");

	for (names = db.next; names != &db; names = names->next) {
		decl = listelement(names, struct declbuilder, list);
		addmember(b, qt, decl->name, align, decl->width);
	}

	if (tok.kind != TRPAREN)
		expect(TCOMMA, "or ')' after declaration");
}

/* 6.7.7 Type names */
struct type *
typename(struct scope *s, enum typequal *tq)
{
	struct qualtype t;

	t = decltype(s, NULL);
	if (t.type && tq) {
			*tq |= t.qual;
	}
	return t.type;
}

static enum linkage
getlinkage(enum declkind kind, enum storageclass sc, struct decl *prior, bool filescope)
{
	if (sc & SCPUBLIC)
		return LINKEXTERN;
	if (sc & SCSTATIC || kind != DECLFUNC)
		return filescope ? LINKINTERN : LINKNONE;
	return prior ? prior->linkage : LINKFUNC;
}

static struct decl *
declcommon(struct scope *s, enum declkind kind, char *name, char *asmname, struct type *t, enum typequal tq, enum storageclass sc, struct decl *prior)
{
	struct decl *d;
	enum linkage linkage;
	const char *kindstr = kind == DECLFUNC ? "function" : "object";
	char *priorname;

	if (prior) {
		if (prior->linkage == LINKNONE)
			error(&tok.loc, "%s '%s' with no linkage redeclared", kindstr, name);
		linkage = getlinkage(kind, sc, prior, s == &filescope);
		if (prior->linkage != linkage) {
			if (prior->linkage != LINKFUNC)
				error(&tok.loc, "%s '%s' redeclared with different linkage", kindstr, name);
			prior->linkage = linkage;
		}
		if (!typecompatible(t, prior->type) || tq != prior->qual)
			error(&tok.loc, "%s '%s' redeclared with incompatible type", kindstr, name);
		if (asmname && strcmp(globalname(prior->value), asmname) != 0)
			error(&tok.loc, "%s '%s' redeclared with different assembler name", kindstr, name);
		prior->type = typecomposite(t, prior->type);
		return prior;
	}
	if (s->parent)
		prior = scopegetdecl(s->parent, name, true);
	linkage = getlinkage(kind, sc, prior, s == &filescope);
	if (linkage != LINKNONE && s->parent) {
		/* XXX: should maintain map of identifiers with linkage to their declaration, and use that */
		if (s->parent != &filescope)
			prior = scopegetdecl(&filescope, name, false);
		if (prior && prior->linkage != LINKNONE) {
			if (prior->kind != kind)
				error(&tok.loc, "'%s' redeclared with different kind", name);
			if (prior->linkage != linkage) {
				if (prior->linkage != LINKFUNC)
					error(&tok.loc, "%s '%s' redeclared with different linkage", kindstr, name);
				prior->linkage = linkage;
			}
			if (!typecompatible(t, prior->type) || tq != prior->qual)
				error(&tok.loc, "%s '%s' redeclared with incompatible type", kindstr, name);
			priorname = globalname(prior->value);
			if (!asmname)
				asmname = priorname;
			else if (strcmp(priorname, asmname) != 0)
				error(&tok.loc, "%s '%s' redeclared with different assembler name", kindstr, name);
			t = typecomposite(t, prior->type);
		}
	}
	d = mkdecl(kind, t, tq, linkage);
	scopeputdecl(s, name, d);
	if (kind == DECLFUNC || linkage != LINKNONE || sc & SCSTATIC)
		d->value = mkglobal(asmname ? asmname : name, linkage == LINKNONE && !asmname);
	return d;
}

static bool
staticassert(struct scope *s)
{
	struct expr *e;
	uint64_t c;

	if (!consume(TSTATIC_ASSERT))
		return false;
	expect(TLPAREN, "after static_assert");
	c = intconstexpr(s, true);
	if (consume(TCOMMA)) {
		e = condexpr(s);
		if (!e->decayed || e->base->kind != EXPRSTRING)
			error(&tok.loc, "expected string literal after static assertion expression");
		if (!c)
			error(&tok.loc, "static assertion failed: %.*s", (int)e->base->string.size, e->base->string.data);
	} else if (!c) {
		error(&tok.loc, "static assertion failed");
	}
	expect(TRPAREN, "after static assertion");
	expect(TSEMICOLON, "after static assertion");
	return true;
}

bool
decl(struct scope *s, struct func *f, bool instmt)
{
	struct qualtype qt;
	struct list *names, db = {&db, &db};
	struct type *pt, *t;
	enum typequal tq;
	enum storageclass sc;
	enum funcspec fs;
	struct init *init;
	struct expr *expr;
	struct declbuilder *decl;
	char *name, *asmname;
	int allowfunc = !f;
	struct decl *d, *prior;
	enum declkind kind;
	int align;
	bool initialize;

	if (staticassert(s))
		return true;
	qt = declaration(s, &sc, &fs, &db, &align, instmt);
	if (!(qt.type || db.next != &db))
		return false;
	if (!f) {
		/* 6.9p2 */
		if (sc & SCAUTO)
			error(&tok.loc, "external declaration must not contain 'auto'");
		if (sc & SCREGISTER)
			error(&tok.loc, "external declaration must not contain 'register'");
	}
	t = qt.type;
	tq = qt.qual;
	kind = t && t->kind == TYPEFUNC ? DECLFUNC : DECLOBJECT;
	if (consume(TASM)) {
		expect(TLPAREN, "after asm");
		asmname = expect(TSTRINGLIT, "for assembler name");
		expect(TRPAREN, "after assembler name");
		allowfunc = 0;
	} else {
		asmname = NULL;
	}
	initialize = kind == DECLOBJECT && consume(TASSIGN);
	for (names = db.next; names != &db; names = names->next) {
		decl = listelement(names, struct declbuilder, list);
		name = decl->name;
		if (decl->width != -1)
			error(&tok.loc, "invalid bit-field declaration");
		if (!t && kind != DECLOBJECT)
			error(&tok.loc, "declaration '%s' with no type", name);
		if (sc & SCTYPEDEF) {
			if (align)
				error(&tok.loc, "typedef '%s' declared with alignment specifier", name);
			if (asmname)
				error(&tok.loc, "typedef '%s' declared with assembler label", name);
			pt = scopegettag(s, name, false);
			if (!pt)
				scopeputtag(s, name, t);
			else if (pt->incomplete)
				*pt = *t;
			else if (!typesame(pt, t))
				error(&tok.loc, "typedef '%s' redefined with different type", name);
			continue;
		}
		prior = scopegetdecl(s, name, false);
		if (prior && prior->kind != kind)
			error(&tok.loc, "'%s' redeclared with different kind", name);
		switch (kind) {
		case DECLOBJECT:
			expr = NULL;
			if (!t) {
				if (!initialize)
					error(&tok.loc, "expected initializer for type-inferred declaration");
				expr = condexpr(s);
				t = expr->type;
				init = mkinit(0, t->size, (struct bitfield){0}, expr);
			}
			d = declcommon(s, kind, name, asmname, t, tq, sc, prior);
			if (d->align < align)
				d->align = align;
			if (expr || initialize) {
				if (f && d->linkage != LINKNONE)
					error(&tok.loc, "object '%s' with block scope and %s linkage cannot have initializer", name, d->linkage == LINKEXTERN ? "external" : "internal");
				if (d->defined)
					error(&tok.loc, "object '%s' redefined", name);
				if (!expr)
					init = parseinit(s, d->type);
			} else {
				init = NULL;
			}
			if (init || d->linkage == LINKNONE) {
				if (d->linkage != LINKNONE || sc & SCSTATIC)
					emitdata(d, init);
				else
					funcinit(f, d, init);
				d->defined = true;
				if (d->tentative.next)
					listremove(&d->tentative);
			} else if (!(sc & SCPUBLIC) && !d->defined && !d->tentative.next) {
				listinsert(tentativedefns.prev, &d->tentative);
			}
			break;
		case DECLFUNC:
			if (align)
				error(&tok.loc, "function '%s' declared with alignment specifier", name);
			t->func.isnoreturn |= fs & FUNCNORETURN;
			if (f && sc && sc != SCPUBLIC)  /* 6.7.1p7 */
				error(&tok.loc, "function '%s' with block scope may only have storage class 'pub'", name);
			d = declcommon(s, kind, name, asmname, t, tq, sc, prior);
			d->inlinedefn = d->linkage == LINKEXTERN && fs & FUNCINLINE && !(sc & SCPUBLIC) && (!prior || prior->inlinedefn);
			if (tok.kind == TLBRACE) {
				if (!allowfunc)
					error(&tok.loc, "function definition not allowed");
				if (d->defined)
					error(&tok.loc, "function '%s' redefined", name);
				if (d->linkage == LINKFUNC)
					d->linkage = LINKINTERN;
				s = mkscope(&filescope);
				f = mkfunc(d, name, t, s);
				stmt(f, s);
				/* XXX: need to keep track of function in case a later declaration specifies extern */
				if (!d->inlinedefn)
					emitfunc(f, d->linkage == LINKEXTERN);
				s = delscope(s);
				delfunc(f);
				d->defined = true;
				return true;
			}
			break;
		}
		if (initialize && !consume(TCOMMA))
			break;
	}
	expect(TSEMICOLON, "after declaration");
	return true;
}

struct decl *stringdecl(struct expr *expr)
{
	static struct map *strings;
	struct mapkey key;
	void **entry;
	struct decl *d;

	if (!strings)
		strings = mkmap(64);
	assert(expr->kind == EXPRSTRING);
	mapkey(&key, expr->string.data, expr->string.size);
	entry = mapput(strings, &key);
	d = *entry;
	if (!d) {
		d = mkdecl(DECLOBJECT, expr->type, QUALNONE, LINKNONE);
		d->value = mkglobal("string", true);
		emitdata(d, mkinit(0, expr->type->size, (struct bitfield){0}, expr));
		*entry = d;
	}
	return d;
}

void
emittentativedefns(void)
{
	struct list *l;

	for (l = tentativedefns.next; l != &tentativedefns; l = l->next)
		emitdata(listelement(l, struct decl, tentative), NULL);
}
