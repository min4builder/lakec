#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"
#include "cc.h"

static char *optable[] = {
	[TINC] = "pp",
	[TDEC] = "mm",
	[TBAND] = "an",
	[TMUL] = "ml",
	[TADD] = "pl",
	[TSUB] = "mi",
	[TBNOT] = "co",
	[TLNOT] = "nt",
	[TDIV] = "dv",
	[TMOD] = "rm",
	[TSHL] = "ls",
	[TSHR] = "rs",
	[TLESS] = "lt",
	[TGREATER] = "gt",
	[TLEQ] = "le",
	[TGEQ] = "ge",
	[TEQL] = "eq",
	[TNEQ] = "ne",
	[TXOR] = "eo",
	[TBOR] = "or",
	[TLAND] = "aa",
	[TLOR] = "oo",
	[TELLIPSIS] = "el",
	[TASSIGN] = "aS"
};

static char *
mangletype(struct type **tstack, int *top, struct type *t, char *w, char *max)
{
	struct param *p;
	struct member *m;
	int i;

	if (max-w < 1)
		return w;

	for (i = 0; i < *top; i++) {
		if (typeequal(&tstack[i]->gen, &t->gen)) {
			*w++ = 'S';
			if (max-w < 1)
				return w;
			if (i == 0) {
				*w++ = '_';
				return w;
			}
			i--;
			/* TODO get S_ syntax working properly for i > 10 */
			w += snprintf(w, max-w, "S%d_", i);
			if (w > max) w = max;
			return w;
		}
	}

	switch(t->kind) {
	case TYPEVOID:
		*w++ = 'v';
		break;
	case TYPEBOOL:
		*w++ = 'b';
		break;
	case TYPECHAR:
		*w++ = t->basic.issigned ? 'c' : 'h';
		break;
	case TYPESHORT:
		*w++ = t->basic.issigned ? 's' : 't';
		break;
	case TYPEENUM:
	case TYPEINT:
		*w++ = t->basic.issigned ? 'i' : 'j';
		break;
	case TYPELONG:
		*w++ = t->basic.issigned ? 'l' : 'm';
		break;
	case TYPELLONG:
		*w++ = t->basic.issigned ? 'x' : 'y';
		break;
	case TYPEFLOAT:
		*w++ = 'f';
		break;
	case TYPEDOUBLE:
		*w++ = 'd';
		break;
	case TYPELDOUBLE:
		*w++ = 'e';
		break;
	case TYPEQUAL:
		if (max-w < 4)
			return w;
		if (t->qual.qual & QUALVOLATILE)
			*w++ = 'V';
		if (!(t->qual.qual & QUALMUT))
			*w++ = 'K';
		w = mangletype(tstack, top, typeeval(t->base), w, max);
		break;
	case TYPEARRAY:
		w = mangletype(tstack, top, typeeval(t->base), w, max);
		break;
	case TYPEPOINTER:
		if (max-w < 3)
			return w;
		*w++ = 'P';
		w = mangletype(tstack, top, typeeval(t->base), w, max);
		tstack[(*top)++] = t;
		break;
	case TYPEFUNC:
		*w++ = 'F';
		w = mangletype(tstack, top, typeeval(t->base), w, max);
		for (p = t->func.params; p; p = p->next)
			w = mangletype(tstack, top, typeeval(p->type), w, max);
		if (t->func.isvararg && max-w >= 1)
			*w++ = 'z';
		if (max-w >= 1)
			*w++ = 'E';
		tstack[(*top)++] = t;
		break;
	case TYPESTRUCT:
	case TYPEUNION:
		if (max-w >= 2) {
			char name[64], *n = name;
			int nt = *top;
			*n++ = '_';
			*n++ = t->kind == TYPESTRUCT ? 'S' : 'U';
			for (m = t->structunion.members; m; m = m->next)
				n = mangletype(tstack, top, typeeval(m->type), n, name + sizeof(name));
			w += snprintf(w, max-w, "%ld%.*s", n-name, (int) (n-name), name);
			if (w > max) w = max;
			*top = nt;
		}
		tstack[(*top)++] = t;
		break;
	}
	return w;
}

char *
mangleuop(enum tokenkind t, struct typegen *t1)
{
	static char n[65];
	struct type *tstack[16];
	int top = 0;
	char *w = n;

	w += snprintf(w, sizeof(n), "_Z%s", optable[t]);
	w = mangletype(tstack, &top, typequal(typeeval(t1), NULL), w, n + sizeof(n) - 1);
	*w = '\0';
	return n;
}

char *
manglebop(enum tokenkind t, struct typegen *t1, struct typegen *t2)
{
	static char n[65];
	struct type *tstack[16];
	int top = 0;
	char *w = n;

	w += snprintf(w, sizeof(n), "_Z%s", optable[t]);
	w = mangletype(tstack, &top, typequal(typeeval(t1), NULL), w, n + sizeof(n) - 1);
	w = mangletype(tstack, &top, typequal(typeeval(t2), NULL), w, n + sizeof(n) - 1);
	*w = '\0';
	return n;
}

char *
manglegen(enum tokenkind t, struct typegen *f)
{
	struct type *tstack[16];
	int top = 0;
	struct param *p;
	char buf[64], *n;
	int len;

	len = 0;
	len += snprintf(buf + len, sizeof(buf) - len, "_Z%s", optable[t]);
	for (p = typeeval(f)->func.params; p; p = p->next) {
		if (len > sizeof(buf))
			break;
		len += mangletype(tstack, &top, typequal(typeeval(p->type), NULL), buf + len, buf + sizeof(buf)) - (buf + len);
	}
	n = malloc(len + 1);
	memcpy(n, buf, len);
	n[len] = '\0';
	return n;
}

