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
	struct typegen *type;
	enum typequal qual;
};

enum storageclass {
	SCNONE,

	SCTYPE        = 1<<1,
	SCPUBLIC      = 1<<2,
	SCSTATIC      = 1<<3,
	SCAUTO        = 1<<4,
	SCREGISTER    = 1<<5,
};

enum funcspec {
	FUNCNONE,

	FUNCINLINE   = 1<<1,
};

struct structbuilder {
	struct type *type;
	struct member **last;
	unsigned bits;  /* number of bits remaining in the last byte */
};

struct declbuilder {
	struct list list;
	char *name, *asmname;
	uint64_t width;
};

struct decl *
mkdecl(enum declkind k, struct typegen *t, enum typequal tq, enum linkage linkage)
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
	case TTYPE:     new = SCTYPE;        break;
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
typequal(enum typequal *tq, struct scope *s, int *align)
{
	struct typegen *t;
	uint64_t i;
	bool isbrack = false;

	switch (tok.kind) {
	case TMUT:      *tq |= QUALMUT;    break;
	case THASH:
		next();
		if (tok.kind == TLBRACK) {
			isbrack = true;
			next();
		}
		if (tok.kind != TIDENT)
			error(&tok.loc, "expecting identifier");
		if (!strcmp("nocopy", tok.lit))
			*tq |= QUALNOCOPY;
		else if (!strcmp("nodrop", tok.lit))
			*tq |= QUALNODROP;
		else if (!strcmp("volatile", tok.lit))
			*tq |= QUALVOLATILE;
		else if (!strcmp("restrict", tok.lit))
			*tq |= QUALRESTRICT;
		else if (isbrack && !strcmp("align", tok.lit)) {
			if (!align)
				error(&tok.loc, "alignment specifier not allowed in this declaration");
			next();
			t = typename(s, NULL);
			if (t) {
				*align = typeeval(t)->align;
			} else {
				i = intconstexpr(s, false);
				if (!i || i & (i - 1) || i > 16)
					error(&tok.loc, "invalid alignment: %d", i);
				*align = (int)i;
			}
			expect(TRBRACK, "to close pragma 'align'");
			return 1;
		} else
			error(&tok.loc, "unknown pragma");
		break;
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
	char *name;
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
	expect(TLPAREN, "after struct/union/enum");
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
		t->qual = QUALNONE;
		t->structunion.members = NULL;
	}
	t->incomplete = true;
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
				if (e->kind != EXPRCONST || !(typeeval(e->type)->prop & PROPINT))
					error(&tok.loc, "expected integer constant expression");
				i = e->constant.i;
				if (typeeval(e->type)->basic.issigned && i >= 1ull << 63) {
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
			d = mkdecl(DECLCONST, &targ->typeint->gen, QUALNONE, LINKNONE);
			d->value = mkintconst(t->repr, i);
			if (i >= 1ull << 31 && i < 1ull << 63) {
				large = true;
				d->type = &targ->typeuint->gen;
			}
			if (large && t->basic.issigned)
				error(&tok.loc, "neither 'int' nor 'unsigned' can represent all enumerator values");
			scopeputdecl(s->parent, name, d);
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

static void
decltype(struct qualtype *qt, struct scope *s, int *align)
{
	struct type **t, *other, *type;
	struct typegen **vars;
	struct expr *e;
	struct param **p;
	enum typequal *tq;
	uint64_t i;

	t = (struct type **) &qt->type;
	tq = &qt->qual;
	if (align)
		*align = 0;
	for (;;) {
		if (typequal(tq, s, align))
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
			*t = (struct type *) scopegettag(s, tok.lit, true);
			if (!*t) {
				if (!importing)
					error(&tok.loc, "type '%s' does not exist", tok.lit);
				*t = mktype(TYPENONE, PROPNONE);
				(*t)->incomplete = true;
				scopeputtag(s->parent, tok.lit, &(*t)->gen);
			}
			*t = typeeval((struct typegen *) *t);
			next();
			if (consume(TLBRACK)) {
				vars = 0;
				i = 0;
				do {
					vars = xreallocarray(vars, sizeof(*vars), i+1);
					vars[i] = typename(s, NULL);
					i++;
				} while(consume(TCOMMA));
				expect(TRBRACK, "to close type function application");
				type = mkapplytype(&(*t)->gen, i, vars);
				type->apply.loc = tok.loc;
				*tq |= (*t)->qual;
				*t = type;
			}
			other = (struct type *) typename(s, NULL);
			if (other) {
				type = mkapplytype(&(*t)->gen, 1, xmalloc(sizeof(*vars)));
				type->apply.vars[0] = (struct typegen *) other;
				type->apply.loc = tok.loc;
				*tq |= (*t)->qual;
				*t = type;
			}
			goto done;
		case TTYPE:
			next();
			expect(TLPAREN, "after 'type' expression");
			e = expr(s);
			*t = typeeval(e->type);
			*tq |= e->qual;
			delexpr(e);
			expect(TRPAREN, "to close 'type' expression");
			goto done;
		case TMUL:
			next();
			*t = mkpointertype(NULL, QUALNONE);
			tq = &(*t)->qual;
			t = (struct type **) &(*t)->base;
			break;
		case TLBRACK:
			next();
			if (tok.kind == TTYPE) {
				next();
				*t = mktype(TYPEPARAM, PROPDERIVED);
				(*t)->qual = QUALNONE;
				(*t)->param.nvars = 0;
				do {
					if (tok.kind != TIDENT)
						error(&tok.loc, "expected identifier in parametric type");
					type = mktype(TYPEVAR, PROPNONE);
					type->incomplete = true;
					type->align = 8;
					type->var.parent = *t;
					type->var.num = (*t)->param.nvars++;
					type->var.isrigid = true;
					scopeputtag(s, tok.lit, &type->gen);
					next();
				} while (consume(TCOMMA));
				expect(TRBRACK, "to close type function declarator");
				tq = &(*t)->qual;
				t = (struct type **) &(*t)->base;
			} else {
				if (tok.kind == TRBRACK) {
					i = 0;
					next();
				} else {
					e = eval(condexpr(s), EVALARITH);
					if (e->kind != EXPRCONST || !(typeeval(e->type)->prop & PROPINT))
						error(&tok.loc, "VLAs are not yet supported");
					i = e->constant.i;
					if (typeeval(e->type)->basic.issigned && i > INT64_MAX)
						error(&tok.loc, "array length must be non-negative");
					delexpr(e);
					expect(TRBRACK, "after array length");
				}
				*t = mkarraytype(NULL, QUALNONE, i);
				tq = &(*t)->qual;
				t = (struct type **) &(*t)->base;
			}
			break;
		case TLPAREN:
			next();
			*t = mktype(TYPEFUNC, PROPDERIVED);
			(*t)->qual = QUALNONE;
			(*t)->func.isvararg = false;
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
			}
			expect(TRPAREN, "to close function declarator");
			tq = &(*t)->qual;
			t = (struct type **) &(*t)->base;
			break;
		case TLBRACE:

		default:
			goto done;
		}
	}
done:
	if (!*t && qt->type)
		error(&tok.loc, "expecting type");
	if (qt->type && qt->qual && ((struct type *) qt->type)->kind == TYPEARRAY) {
		qt->type = &mkarraytype(((struct type *) qt->type)->base, ((struct type *) qt->type)->qual | qt->qual, ((struct type *) qt->type)->array.length)->gen;
		qt->qual = QUALNONE;
	}
	for (type = (struct type *) qt->type; type && type->prop & PROPDERIVED; type = (struct type *) type->base) {
		if (type->prop & PROPDERIVED && !type->base)
			error(&tok.loc, "expected type");
		switch (type->kind) {
		case TYPEARRAY:
			if (typeeval(type->base)->incomplete)
				error(&tok.loc, "array element has incomplete type");
			type->align = 0;
			type->size = type->array.length;
			for (other = typeeval(type->base); other; other = typeeval(other->base)) {
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
			if (typeeval(type->base)->kind == TYPEFUNC)
				error(&tok.loc, "function declarator specifies function return type");
			if (typeeval(type->base)->kind == TYPEARRAY)
				error(&tok.loc, "function declarator specifies array return type");
			break;
		case TYPEAPPLY:
		case TYPEPARAM:
		case TYPEPOINTER:
			break;
		default:
			return;
		}
	}
}

static struct declbuilder *
mkdeclbuilder(char *name, char *asmname, uint64_t width)
{
	struct declbuilder *db;

	db = xmalloc(sizeof(*db));
	memset(db, 0, sizeof(*db));
	db->name = name;
	db->asmname = asmname;
	db->width = width;

	return db;
}

static struct qualtype
declaration(struct scope *s, enum storageclass *sc, enum funcspec *fs, struct list *db, int *align, bool needsc)
{
	struct qualtype qt = { NULL, QUALNONE };
	struct declbuilder *cur;
	enum tokenkind overload = TNONE;
	char *name, *asmname = 0;
	int64_t width;

	if (sc)
		*sc = SCNONE;
	if (fs)
		*fs = FUNCNONE;

	if (tok.kind == THASH) {
		next();
		expect(TLBRACK, "for declaration pragma");
		if (tok.kind != TIDENT)
			error(&tok.loc, "expected identifier");
		if (!strcmp("name", tok.lit)) {
			next();
			asmname = expect(TSTRINGLIT, "for external name");
			expect(TRBRACK, "to close pragma 'name'");
		} else
			error(&tok.loc, "unknown pragma");
	}

	storageclass(sc);
	funcspec(fs);
	if (sc && !*sc && (!fs || !*fs) && !asmname && needsc)
		return (struct qualtype){NULL, QUALNONE};

	if (sc && *sc & SCTYPE) {
		qt.type = &mktype(TYPENONE, PROPNONE)->gen;
		((struct type *) qt.type)->incomplete = true;
	}

	switch (tok.kind) {
	case TENUM:
		break;
	case TIDENT:
		do {
			if (tok.kind != TIDENT)
				error(&tok.loc, "expected identifier");
			name = tok.lit;
			next();
			width = -1;
			if (consume(TCOLON)) {
				typequal(&qt.qual, s, align);
				width = intconstexpr(s, true);
				if (width < 0) {
					width = -width;
					qt.type = &targ->typeint->gen;
				} else {
					qt.type = &targ->typeuint->gen;
				}
				if (qt.type == &targ->typeint->gen && width >= targ->typeint->size * 8)
					qt.type = &targ->typelong->gen;
				else if (qt.type == &targ->typeuint->gen && width > targ->typeuint->size * 8)
					qt.type = &targ->typeulong->gen;
			}
			if (sc && *sc & SCTYPE)
				scopeputtag(s, name, qt.type);
			cur = mkdeclbuilder(name, asmname, width);
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
	default:
		return (struct qualtype){NULL, QUALNONE};
	}

	decltype(&qt, s, align);

	if (overload) {
		if (typeeval(qt.type)->kind != TYPEFUNC)
			error(&tok.loc, "overload must be non-generic function");
		cur = mkdeclbuilder(manglegen(overload, qt.type), 0, -1);
		listinsert(db->prev, &cur->list);
	}
	return qt;
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
		*p = mkparam(decl->name, t.type, t.qual);
		p = &(*p)->next;
	}

	return p;
}

static void
addmember(struct structbuilder *b, struct qualtype mt, char *name, int align, uint64_t width)
{
	struct type *t = b->type;
	struct type *type = typeeval(mt.type);
	struct member *m;
	size_t end;

	// XXX: check incomplete type, except for flexible array member
	if (type->kind == TYPEFUNC)
		error(&tok.loc, "struct member '%s' has function type", name);
	assert(type->align > 0);
	if (strcmp(name, "_") || width == -1) {
		m = xmalloc(sizeof(*m));
		m->type = &type->gen;
		m->qual = mt.qual;
		t->qual |= m->qual & (QUALNOCOPY | QUALNODROP);
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
		if (align < type->align)
			align = type->align;
		t->size = ALIGNUP(t->size, align);
		if (t->kind == TYPESTRUCT) {
			m->offset = t->size;
			t->size += type->size;
		} else {
			m->offset = 0;
			if (t->size < type->size)
				t->size = type->size;
		}
		b->bits = 0;
	} else {  /* bit-field */
		if (!width && strcmp(name, "_"))
			error(&tok.loc, "bit-field with zero width must be unnamed");
		if (width > type->size * 8)
			error(&tok.loc, "bit-field exceeds width of underlying type");
		align = type->align;
		if (t->kind == TYPESTRUCT) {
			/* calculate end of the storage-unit for this bit-field */
			end = ALIGNUP(t->size, type->size);
			if (!width || width > (end - t->size) * 8 + b->bits) {
				/* no room, allocate a new storage-unit */
				t->size = end;
				b->bits = 0;
			}
			if (m) {
				m->offset = ALIGNDOWN(t->size - !!b->bits, type->size);
				m->bits.before = (t->size - m->offset) * 8 - b->bits;
				m->bits.after = type->size * 8 - width - m->bits.before;
			}
			t->size += (width - b->bits + 7) / 8;
			b->bits = (b->bits - width) % 8;
		} else if (m) {
			m->offset = 0;
			m->bits.before = 0;
			m->bits.after = type->size * 8 - width;
			if (t->size < type->size)
				t->size = type->size;
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
struct typegen *
typename(struct scope *s, enum typequal *tq)
{
	struct qualtype t = { NULL, QUALNONE };

	decltype(&t, s, NULL);
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
declcommon(struct scope *s, enum declkind kind, char *name, char *asmname, struct typegen *t, enum typequal tq, enum storageclass sc, struct decl *prior)
{
	struct decl *d;
	enum linkage linkage;
	const char *kindstr = kind == DECLFUNC ? "function" : "object";
	char *priorname;

	if (prior) {
		if (prior->linkage == LINKNONE)
			error(&tok.loc, "%s '%s' with no linkage redeclared", kindstr, name);
		linkage = getlinkage(kind, sc, prior, s->parent == &filescope);
		if (prior->linkage != linkage) {
			if (prior->linkage != LINKFUNC)
				error(&tok.loc, "%s '%s' redeclared with different linkage", kindstr, name, linkage, prior->linkage);
			prior->linkage = linkage;
		}
		if (!typeequal(t, prior->type) || tq != prior->qual)
			error(&tok.loc, "%s '%s' redeclared with different type", kindstr, name);
		if (asmname && strcmp(globalname(prior->value), asmname) != 0)
			error(&tok.loc, "%s '%s' redeclared with different assembler name", kindstr, name);
		prior->type = typecomposite(t, prior->type);
		return prior;
	}
	if (s->parent->parent)
		prior = scopegetdecl(s->parent->parent, name, true);
	linkage = getlinkage(kind, sc, prior, s->parent == &filescope);
	if (linkage != LINKNONE && s->parent->parent) {
		/* XXX: should maintain map of identifiers with linkage to their declaration, and use that */
		if (s->parent->parent != &filescope)
			prior = scopegetdecl(&filescope, name, false);
		if (prior && prior->linkage != LINKNONE) {
			if (prior->kind != kind)
				error(&tok.loc, "'%s' redeclared with different kind", name);
			if (prior->linkage != linkage) {
				if (prior->linkage != LINKFUNC)
					error(&tok.loc, "%s '%s' redeclared with different linkage", kindstr, name);
				prior->linkage = linkage;
			}
			if (!typeequal(t, prior->type) || tq != prior->qual)
				error(&tok.loc, "%s '%s' redeclared with different type", kindstr, name);
			priorname = globalname(prior->value);
			if (!asmname)
				asmname = priorname;
			else if (strcmp(priorname, asmname) != 0)
				error(&tok.loc, "%s '%s' redeclared with different assembler name", kindstr, name);
			t = typecomposite(t, prior->type);
		}
	}
	d = mkdecl(kind, t, tq, linkage);
	scopeputdecl(s->parent, name, d);
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
		if (e->kind != EXPRSTRING)
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
	struct typegen *pt, *t;
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
	s = mkscope(s);
	qt = declaration(s, &sc, &fs, &db, &align, instmt);
	if (!(qt.type || db.next != &db)) {
		s = delscope(s);
		return false;
	}
	if (!f) {
		/* 6.9p2 */
		if (sc & SCAUTO)
			error(&tok.loc, "external declaration must not contain 'auto'");
		if (sc & SCREGISTER)
			error(&tok.loc, "external declaration must not contain 'register'");
	}
	pt = t = qt.type;
	tq = qt.qual;
	while (pt && typeeval(pt)->kind == TYPEPARAM)
		pt = typeeval(pt)->base;
	kind = pt && typeeval(pt)->kind == TYPEFUNC ? DECLFUNC : DECLOBJECT;
	initialize = kind == DECLOBJECT && consume(TASSIGN);
	for (names = db.next; names != &db; names = names->next) {
		decl = listelement(names, struct declbuilder, list);
		name = decl->name;
		asmname = decl->asmname;
		if (decl->width != -1)
			error(&tok.loc, "invalid bit-field declaration");
		if (!t && kind != DECLOBJECT)
			error(&tok.loc, "declaration '%s' with no type", name);
		if (sc & SCTYPE) {
			if (align)
				error(&tok.loc, "type '%s' declared with alignment specifier", name);
			if (asmname)
				error(&tok.loc, "type '%s' declared with assembler label", name);
			pt = scopegettag(s->parent, name, false);
			if (!pt)
				scopeputtag(s->parent, name, t);
			else if (typeeval(pt)->incomplete)
				*(struct type *) pt = *(struct type *) t;
			else if (!typeequal(pt, t))
				error(&tok.loc, "type '%s' redefined with different type", name);
			continue;
		}
		prior = scopegetdecl(s->parent, name, false);
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
				init = mkinit(0, typeeval(t)->size, (struct bitfield){0}, expr);
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
					funcinit(f, d, init, name);
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
				f = mkfunc(d, name, typeeval(pt), s);
				s->func = f;
				stmt(f, s);
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
	s = delscope(s);
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
		emitdata(d, mkinit(0, typeeval(expr->type)->size, (struct bitfield){0}, expr));
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
