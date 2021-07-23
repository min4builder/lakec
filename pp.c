#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "cc.h"

struct macroparam {
	char *name;
	enum {
		PARAMTOK = 1<<0,  /* the parameter is used normally */
		PARAMSTR = 1<<1,  /* the parameter is used with the '#' operator */
		PARAMVAR = 1<<2,  /* the parameter is variadic */
	} flags;
};

struct macroarg {
	struct token *token;
	size_t ntoken;
	/* stringized argument */
	struct token str;
};

struct macro {
	enum {
		MACROOBJ,
		MACROFUNC,
		MACROTYPEFUNC,
	} kind;
	char *name;
	/* whether or not this macro is ineligible for expansion */
	bool hide;
	/* parameters of function-like macro */
	struct macroparam *param;
	size_t nparam;
	/* argument tokens of macro invocation */
	struct macroarg *arg;
	/* replacement list */
	struct token *token;
	size_t ntoken;
};

struct frame {
	struct token *token;
	size_t ntoken;
	struct macro *macro;
};

enum ppflags ppflags;
bool importing;

static struct array dirs;
static struct array ctx;
static struct map *macros;
/* number of macros currently undergoing expansion */
static size_t macrodepth;

void
ppdir(char *name)
{
	char **d;

	d = arrayadd(&dirs, sizeof(*d));
	*d = name;
}

void
ppinit(void)
{
	macros = mkmap(64);
	next();
}

/* check if two macro definitions are equal, as in C11 6.10.3p2 */
static bool
macroequal(struct macro *m1, struct macro *m2)
{
	struct macroparam *p1, *p2;
	struct token *t1, *t2;

	if (m1->kind != m2->kind)
		return false;
	if (m1->kind == MACROFUNC || m1->kind == MACROTYPEFUNC) {
		if (m1->nparam != m2->nparam)
			return false;
		for (p1 = m1->param, p2 = m2->param; p1 < m1->param + m1->nparam; ++p1, ++p2) {
			if (strcmp(p1->name, p2->name) != 0 || p1->flags != p2->flags)
				return false;
		}
	}
	if (m1->ntoken != m2->ntoken)
		return false;
	for (t1 = m1->token, t2 = m2->token; t1 < m1->token + m1->ntoken; ++t1, ++t2) {
		if (t1->kind != t2->kind)
			return false;
		if (t1->lit && strcmp(t1->lit, t2->lit) != 0)
			return false;
	}
	return true;
}

/* find the index of a macro parameter with the given name */
static size_t
macroparam(struct macro *m, struct token *t)
{
	size_t i;

	if (t->kind == TIDENT) {
		for (i = 0; i < m->nparam; ++i) {
			if (strcmp(m->param[i].name, t->lit) == 0)
				return i;
		}
	}
	return -1;
}

/* lookup a macro by name */
static struct macro *
macroget(char *name)
{
	struct mapkey k;

	mapkey(&k, name, strlen(name));
	return mapget(macros, &k);
}

static void
macrodone(struct macro *m)
{
	m->hide = false;
	if ((m->kind == MACROFUNC || m->kind == MACROTYPEFUNC) && m->nparam > 0)
		free(m->arg);
	--macrodepth;
}

static struct token *
framenext(struct frame *f)
{
	return f->ntoken--, f->token++;
}

/* push a new context frame */
static struct frame *
ctxpush(struct token *t, size_t n, struct macro *m, bool space)
{
	struct frame *f;

	f = arrayadd(&ctx, sizeof(*f));
	f->token = t;
	f->ntoken = n;
	f->macro = m;
	if (n > 0)
		t[0].space = space;
	return f;
}

/* get the next token from the context */
static struct token *
ctxnext(void)
{
	struct frame *f;
	struct macro *m;
	bool space;
	size_t i;

again:
	for (f = arraylast(&ctx, sizeof(*f)); ctx.len; --f, ctx.len -= sizeof(*f)) {
		if (f->ntoken)
			break;
		if (f->macro)
			macrodone(f->macro);
	}
	if (ctx.len == 0)
		return NULL;
	m = f->macro;
	if (m && (m->kind == MACROFUNC || m->kind == MACROTYPEFUNC)) {
		/* try to expand macro parameter */
		space = f->token->space;
		switch (f->token->kind) {
		case TIDENT:
			i = macroparam(m, f->token);
			if (i == -1)
				break;
			framenext(f);
			if (m->arg[i].ntoken == 0)
				goto again;
			f = ctxpush(m->arg[i].token, m->arg[i].ntoken, NULL, space);
			break;
		}
		/* XXX: token concatenation */
	}
	return framenext(f);
}

static void
import(void)
{
	char fullname[1024];
	FILE *file;
	char *name;
	char **dir;
	int i;

	if (importing)
		error(&tok.loc, "cannot import from header");
	importing = true;

	name = tokencheck(&tok, TSTRINGLIT, "after 'import'");
	name++;
	for (i = 0; name[i] != '"'; i++) ;
	name[i] = '\0';
	arrayforeach (&dirs, dir) {
		snprintf(fullname, sizeof(fullname), "%s/%s", *dir, name);
		file = fopen(fullname, "r");
		if (file) {
			scan(&tok); /* semicolon */
			scanfrom(name, file);
			return;
		}
	}
	error(&tok.loc, "import not found");
}

static void
define(void)
{
	struct token *t;
	enum tokenkind end;
	struct macro *m;
	struct macroparam *p;
	struct array params = {0}, repl = {0};
	struct mapkey k;
	void **entry;
	size_t i, bn;

	m = xmalloc(sizeof(*m));
	m->name = tokencheck(&tok, TIDENT, "after #define");
	m->hide = false;
	t = arrayadd(&repl, sizeof(*t));
	scan(t);
	if (t->kind == TLPAREN || t->kind == TLBRACK) {
		m->kind = t->kind == TLPAREN ? MACROFUNC : MACROTYPEFUNC;
		if (t->kind == TLPAREN)
			end = TRPAREN;
		else
			end = TRBRACK;
		/* read macro parameter names */
		scan(&tok);
		while (tok.kind != end) {
			if (params.len) {
				if (p->flags & PARAMVAR)
					tokencheck(&tok, TRPAREN, "after '...'");
				tokencheck(&tok, TCOMMA, "or end of argument list after macro parameter");
				scan(&tok);
			}
			p = arrayadd(&params, sizeof(*p));
			p->flags = 0;
			p->name = tokencheck(&tok, TIDENT, "of macro parameter name");
			scan(&tok);
			if (tok.kind == TELLIPSIS) {
				p->flags |= PARAMVAR;
				scan(&tok);
			}
		}
		scan(t);  /* first token in replacement list */
	} else if (t->kind == TASSIGN) {
		m->kind = MACROOBJ;
	} else {
		error(&t->loc, "expected '=' in object-like macro definition");
	}
	m->param = params.val;
	m->nparam = params.len / sizeof(m->param[0]);

	/* read macro body */
	if (t->kind == TASSIGN)
		end = TSEMICOLON;
	else if (t->kind == TLBRACE)
		end = TRBRACE;
	else
		error(&t->loc, "expecting '=' or '{' to start macro body");
	scan(t);
	bn = 0;
	i = macroparam(m, t);
	while ((t->kind != end || bn) && t->kind != TEOF) {
		if (t->kind == TLBRACE)
			bn++;
		else if (t->kind == TRBRACE)
			bn--;
		t = arrayadd(&repl, sizeof(*t));
		scan(t);
		if (m->kind != MACROFUNC && m->kind != MACROTYPEFUNC)
			continue;
		if (i != -1)
			m->param[i].flags |= PARAMTOK;
		i = macroparam(m, t);
	}
	tokencheck(t, end, "to end macro body");
	m->token = repl.val;
	m->ntoken = repl.len / sizeof(*t) - 1;
	scan(&tok);

	mapkey(&k, m->name, strlen(m->name));
	entry = mapput(macros, &k);
	if (*entry && !macroequal(m, *entry))
		error(&tok.loc, "redefinition of macro '%s'", m->name);
	*entry = m;
}

static void
undef(void)
{
	char *name;
	struct mapkey k;
	void **entry;
	struct macro *m;

	name = tokencheck(&tok, TIDENT, "after undef");
	mapkey(&k, name, strlen(name));
	entry = mapput(macros, &k);
	m = *entry;
	if (m) {
		free(name);
		free(m->param);
		free(m->token);
		*entry = NULL;
	}
	scan(&tok);
}

static bool
directive(void)
{
	enum ppflags oldflags;
	char *name;

	if (tok.kind != TIDENT)
		return false;
	name = tok.lit;
	oldflags = ppflags;
	ppflags |= PPNEWLINE;
	if (strcmp(name, "import") == 0) {
		scan(&tok);
		import();
		tokencheck(&tok, TSEMICOLON, "after import");
	} else if (strcmp(name, "define") == 0) {
		scan(&tok);
		define();
	} else if (strcmp(name, "undef") == 0) {
		scan(&tok);
		undef();
		tokencheck(&tok, TSEMICOLON, "after undef");
	} else if (strcmp(name, "line") == 0) {
		error(&tok.loc, "line directive is not implemented");
	} else {
		ppflags = oldflags;
		return false;
	}
	free(name);
	ppflags = oldflags;
	return true;
}

/* get the next token without expanding it */
static void
nextinto(struct token *t)
{
	static bool newline = true;

	for (;;) {
		scan(t);
		if (!(newline && directive())) {
			newline = tok.kind == TNEWLINE;
			break;
		}
	}
}

static struct token *
rawnext(void)
{
	struct token *t;

	t = ctxnext();
	if (!t) {
		t = &tok;
		nextinto(t);
	}
	return t;
}

static bool
peekparen(enum tokenkind begin)
{
	static struct array pending;
	struct token *t;
	struct frame *f;

	t = ctxnext();
	if (t) {
		if (t->kind == begin)
			return true;
		f = arraylast(&ctx, sizeof(*f));
		--f->token;
		++f->ntoken;
		return false;
	}
	pending.len = 0;
	do t = arrayadd(&pending, sizeof(*t)), nextinto(t);
	while (t->kind == TNEWLINE);
	if (t->kind == begin)
		return true;
	t = pending.val;
	ctxpush(t, pending.len / sizeof(*t), NULL, t[0].space);
	return false;
}

static void
stringize(struct array *buf, struct token *t)
{
	const char *lit;

	if ((t->space || t->kind == TNEWLINE) && buf->len > 1 && ((char *)buf->val)[buf->len - 1] != ' ')
		arrayaddbuf(buf, " ", 1);
	lit = t->lit ? t->lit : tokstr[t->kind];
	if (t->kind == TSTRINGLIT || t->kind == TCHARCONST) {
		for (; *lit; ++lit) {
			if (*lit == '\\' || *lit == '"')
				arrayaddbuf(buf, "\\", 1);
			arrayaddbuf(buf, lit, 1);
		}
	} else if (lit) {
		arrayaddbuf(buf, lit, strlen(lit));
	}
}

static bool
typefuncend(enum tokenkind t)
{
	switch (t) {
	case TLBRACE:
	case TASSIGN:
	case TSEMICOLON:
	case TCOMMA:
	case TRPAREN:
	case TRBRACK:
	case TEOF:
		return true;
	}
	return false;
}

static bool
expand(struct token *t)
{
	static struct token pending;
	struct macro *m;
	struct macroparam *p;
	struct macroarg *arg;
	struct array str, tok;
	enum tokenkind begin, end;
	size_t i, depth, paren;
	bool space, braced;

	if (t->kind != TIDENT)
		return false;
	m = macroget(t->lit);
	if (!m || m->hide || t->hide) {
		t->hide = true;
		return false;
	}
	space = t->space;
	braced = true;
	if (m->kind == MACROFUNC || m->kind == MACROTYPEFUNC) {
		begin = m->kind == MACROFUNC ? TLPAREN : TLBRACK;
		end = m->kind == MACROFUNC ? TRPAREN : TRBRACK;
		braced = peekparen(begin);
		if (!braced && (m->kind == MACROFUNC || m->nparam != 1))
			return false;
		/* read macro arguments */
		paren = 0;
		depth = macrodepth;
		tok = (struct array){0};
		arg = xreallocarray(NULL, m->nparam, sizeof(*arg));
		t = rawnext();
		for (i = 0; i < m->nparam; ++i) {
			p = &m->param[i];
			if (p->flags & PARAMSTR) {
				str = (struct array){0};
				arrayaddbuf(&str, "\"", 1);
			}
			arg[i].ntoken = 0;
			for (;;) {
				if (t->kind == TEOF)
					error(&t->loc, "EOF when reading macro parameters");
				if (macrodepth <= depth) {
					/* adjust current macro depth, in case it got shallower */
					depth = macrodepth;
					if (paren == 0 && (!braced && typefuncend(t->kind)
					  || t->kind == end
					  || t->kind == TCOMMA && !(p->flags & PARAMVAR)))
						break;
					switch (t->kind) {
					case TLBRACK:
					case TLPAREN: ++paren; break;
					case TRBRACK:
					case TRPAREN: --paren; break;
					}
					if (p->flags & PARAMSTR)
						stringize(&str, t);
				}
				if (p->flags & PARAMTOK && !expand(t)) {
					arrayaddbuf(&tok, t, sizeof(*t));
					++arg[i].ntoken;
				}
				t = rawnext();
			}
			if (p->flags & PARAMSTR) {
				arrayaddbuf(&str, "\"", 2);
				arg[i].str = (struct token){
					.kind = TSTRINGLIT,
					.lit = str.val,
				};
			}
			if (!braced || t->kind == end)
				break;
			t = rawnext();
		}
		if (i + 1 < m->nparam)
			error(&t->loc, "not enough arguments for macro '%s'", m->name);
		if (braced && t->kind != end)
			error(&t->loc, "too many arguments for macro '%s'", m->name);
		if (!braced)
			pending = *t;
		for (i = 0, t = tok.val; i < m->nparam; ++i) {
			arg[i].token = t;
			t += arg[i].ntoken;
		}
		m->arg = arg;
	}
	if (!braced)
		ctxpush(&pending, 1, NULL, pending.space);
	ctxpush(m->token, m->ntoken, m, space);
	m->hide = true;
	++macrodepth;
	return true;
}

static void
keyword(struct token *tok)
{
	static const struct {
		const char *name;
		int value;
	} keywords[] = {
		{"_Generic",       T_GENERIC},
		{"__attribute__",  T__ATTRIBUTE__},
		{"alignof",        TALIGNOF},
		{"auto",           TAUTO},
		{"break",          TBREAK},
		{"case",           TCASE},
		{"continue",       TCONTINUE},
		{"do",             TDO},
		{"else",           TELSE},
		{"enum",           TENUM},
		{"for",            TFOR},
		{"goto",           TGOTO},
		{"if",             TIF},
		{"inline",         TINLINE},
		{"mut",            TMUT},
		{"pub",            TPUB},
		{"register",       TREGISTER},
		{"return",         TRETURN},
		{"sizeof",         TSIZEOF},
		{"static",         TSTATIC},
		{"static_assert",  TSTATIC_ASSERT},
		{"struct",         TSTRUCT},
		{"switch",         TSWITCH},
		{"type",           TTYPE},
		{"union",          TUNION},
		{"void",           TVOID},
		{"while",          TWHILE},
	};
	size_t low = 0, high = LEN(keywords), mid;
	int cmp;

	while (low < high) {
		mid = (low + high) / 2;
		cmp = strcmp(tok->lit, keywords[mid].name);
		if (cmp == 0) {
			tok->kind = keywords[mid].value;
			tok->lit = NULL;
			break;
		}
		if (cmp < 0)
			high = mid;
		else
			low = mid + 1;
	}
}

void
next(void)
{
	struct token *t;

	do t = rawnext();
	while (expand(t) || t->kind == TNEWLINE && !(ppflags & PPNEWLINE));
	tok = *t;
	if (tok.kind == TIDENT)
		keyword(&tok);
}

bool
peek(int kind)
{
	static struct token pending;
	struct token old;

	old = tok;
	next();
	if (tok.kind == kind) {
		next();
		return true;
	}
	pending = tok;
	tok = old;
	ctxpush(&pending, 1, NULL, pending.space);
	return false;
}

char *
expect(enum tokenkind kind, const char *msg)
{
	char *lit;

	lit = tokencheck(&tok, kind, msg);
	next();

	return lit;
}

bool
consume(int kind)
{
	if (tok.kind != kind)
		return false;
	next();
	return true;
}
