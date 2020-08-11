#include <stdio.h>

struct func;

enum tokenkind {
	TNONE,

	TEOF,
	TNEWLINE,
	TOTHER,

	TIDENT,
	TNUMBER,
	TCHARCONST,
	TSTRINGLIT,

	/* keyword */
	TALIGNAS,
	TALIGNOF,
	TASM,
	TAUTO,
	TBREAK,
	TCASE,
	TCONST,
	TCONTINUE,
	TDEFAULT,
	TDO,
	TELSE,
	TENUM,
	TFOR,
	TGOTO,
	TIF,
	TINLINE,
	TNORETURN,
	TPUB,
	TREGISTER,
	TRESTRICT,
	TRETURN,
	TSIZEOF,
	TSTATIC,
	TSTATIC_ASSERT,
	TSTRUCT,
	TSWITCH,
	TTYPEDEF,
	TTYPEOF,
	TUNION,
	TVOID,
	TVOLATILE,
	TWHILE,
	T_GENERIC,
	T__ATTRIBUTE__,

	/* punctuator */
	TLBRACK,
	TRBRACK,
	TLPAREN,
	TRPAREN,
	TLBRACE,
	TRBRACE,
	TPERIOD,
	TARROW,
	TINC,
	TDEC,
	TBAND,
	TMUL,
	TADD,
	TSUB,
	TBNOT,
	TLNOT,
	TDIV,
	TMOD,
	TSHL,
	TSHR,
	TLESS,
	TGREATER,
	TLEQ,
	TGEQ,
	TEQL,
	TNEQ,
	TXOR,
	TBOR,
	TLAND,
	TLOR,
	TQUESTION,
	TCOLON,
	TCOLONCOLON,
	TSEMICOLON,
	TELLIPSIS,
	TASSIGN,
	TMULASSIGN,
	TDIVASSIGN,
	TMODASSIGN,
	TADDASSIGN,
	TSUBASSIGN,
	TSHLASSIGN,
	TSHRASSIGN,
	TBANDASSIGN,
	TXORASSIGN,
	TBORASSIGN,
	TCOMMA,
	THASH,
	THASHHASH,
};

struct location {
	const char *file;
	size_t line, col;
};

struct token {
	enum tokenkind kind;
	/* whether or not the token is ineligible for expansion */
	_Bool hide;
	/* whether or not the token was preceeded by a space */
	_Bool space;
	struct location loc;
	char *lit;
};

enum typequal {
	QUALNONE,

	QUALCONST    = 1<<1,
	QUALRESTRICT = 1<<2,
	QUALVOLATILE = 1<<3,
	QUALATOMIC   = 1<<4,
};

enum typekind {
	TYPENONE,

	TYPEVOID,
	TYPEBOOL,
	TYPECHAR,
	TYPESHORT,
	TYPEINT,
	TYPEENUM,
	TYPELONG,
	TYPELLONG,
	TYPEFLOAT,
	TYPEDOUBLE,
	TYPELDOUBLE,
	TYPEPOINTER,
	TYPEARRAY,
	TYPEFUNC,
	TYPESTRUCT,
	TYPEUNION,
};

enum typeprop {
	PROPNONE,

	PROPOBJECT  = 1<<0,
	PROPCHAR    = 1<<1,
	PROPINT     = 1<<2,
	PROPREAL    = 1<<3,
	PROPARITH   = 1<<4,
	PROPSCALAR  = 1<<5,
	PROPAGGR    = 1<<6,
	PROPDERIVED = 1<<7,
	PROPFLOAT   = 1<<8,
};

struct param {
	char *name;
	struct type *type;
	enum typequal qual;
	struct value *value;
	struct param *next;
};

struct bitfield {
	short before;  /* number of bits in the storage unit before the bit-field */
	short after;   /* number of bits in the storage unit after the bit-field */
};

struct member {
	char *name;
	struct type *type;
	enum typequal qual;
	uint64_t offset;
	struct bitfield bits;
	struct member *next;
};

struct type {
	enum typekind kind;
	enum typeprop prop;
	int align;
	uint64_t size;
	struct repr *repr;
	union {
		struct type *base;
		struct list link;  /* used only during construction of type */
	};
	/* qualifiers of the base type */
	enum typequal qual;
	_Bool incomplete;
	union {
		struct {
			_Bool issigned, iscomplex;
		} basic;
		struct {
			uint64_t length;
		} array;
		struct {
			_Bool isvararg, isnoreturn, paraminfo;
			struct param *params;
		} func;
		struct {
			char *tag;
			struct member *members;
		} structunion;
	};
};

enum declkind {
	DECLTYPE,
	DECLOBJECT,
	DECLFUNC,
	DECLCONST,
	DECLBUILTIN,
};

enum linkage {
	LINKNONE,
	LINKFUNC,
	LINKINTERN,
	LINKEXTERN,
};

enum builtinkind {
	BUILTINALLOCA,
	BUILTINCONSTANTP,
	BUILTINEXPECT,
	BUILTININFF,
	BUILTINNANF,
	BUILTINOFFSETOF,
	BUILTINTYPESCOMPATIBLEP,
	BUILTINVAARG,
	BUILTINVACOPY,
	BUILTINVAEND,
	BUILTINVALIST,
	BUILTINVASTART,
};

struct decl {
	enum declkind kind;
	enum linkage linkage;
	struct type *type;
	enum typequal qual;
	struct value *value;
	_Bool defined;

	/* link in list of tentative object definitions */
	struct list tentative;
	/* alignment of object storage (may be stricter than type requires) */
	int align;

	/* the function might have an "inline definition" (C11 6.7.4p7) */
	_Bool inlinedefn;

	enum builtinkind builtin;
};

struct scope {
	struct map *tags;
	struct map *decls;
	struct value *breaklabel;
	struct value *continuelabel;
	struct switchcases *switchcases;
	struct scope *parent;
};

enum exprkind {
	/* primary expression */
	EXPRIDENT,
	EXPRCONST,
	EXPRSTRING,

	/* postfix expression */
	EXPRCALL,
	/* member E.M gets transformed to *(typeof(E.M) *)((char *)E + offsetof(typeof(E), M)) */
	EXPRBITFIELD,
	EXPRINCDEC,
	EXPRCOMPOUND,
	/* subscript E1[E2] gets transformed to *((E1)+(E2)) */

	EXPRUNARY,
	EXPRCAST,
	EXPRBINARY,
	EXPRCOND,
	EXPRASSIGN,
	EXPRCOMMA,

	EXPRBUILTIN,
	EXPRTEMP,
};

struct expr {
	enum exprkind kind;
	/* whether this expression is an lvalue */
	_Bool lvalue;
	/* whether this expression is a pointer decayed from an array or function designator */
	_Bool decayed;
	/* the unqualified type of the expression */
	struct type *type;
	/* the type qualifiers of the object this expression refers to (ignored for non-lvalues) */
	enum typequal qual;
	enum tokenkind op;
	struct expr *base;
	struct expr *next;
	union {
		struct {
			struct decl *decl;
		} ident;
		union {
			uint64_t i;
			double f;
		} constant;
		struct {
			char *data;
			size_t size;
		} string;
		struct {
			struct expr *args;
			size_t nargs;
		} call;
		struct {
			struct bitfield bits;
		} bitfield;
		struct {
			struct init *init;
		} compound;
		struct {
			_Bool post;
		} incdec;
		struct {
			struct expr *l, *r;
		} binary;
		struct {
			struct expr *t, *f;
		} cond;
		struct {
			struct expr *l, *r;
		} assign;
		struct {
			enum builtinkind kind;
		} builtin;
		struct value *temp;
	};
};

struct init {
	uint64_t start, end;
	struct expr *expr;
	struct bitfield bits;
	struct init *next;
};

/* token */

extern struct token tok;
extern const char *tokstr[];

void tokenprint(const struct token *);
char *tokencheck(const struct token *, enum tokenkind, const char *);
_Noreturn void error(const struct location *, const char *, ...);

/* scan */

void scanfrom(const char *, FILE *);
void scanopen(void);
void scan(struct token *);

/* preprocessor */

enum ppflags {
	/* preserve newlines in preprocessor output */
	PPNEWLINE   = 1 << 0,
};

extern enum ppflags ppflags;

void ppdir(char *);
void ppinit(void);

void next(void);
_Bool peek(int);
char *expect(enum tokenkind, const char *);
_Bool consume(int);

/* type */

struct type *mktype(enum typekind, enum typeprop);
struct type *mkpointertype(struct type *, enum typequal);
struct type *mkarraytype(struct type *, enum typequal, uint64_t);

_Bool typecompatible(struct type *, struct type *);
_Bool typeconvertible(struct type *, struct type *);
_Bool typesame(struct type *, struct type *);
struct type *typecomposite(struct type *, struct type *);
struct type *typeunqual(struct type *, enum typequal *);
struct type *typecommonreal(struct type *, unsigned, struct type *, unsigned);
struct type *typepromote(struct type *, unsigned);
enum typeprop typeprop(struct type *);
struct member *typemember(struct type *, const char *, uint64_t *);

struct param *mkparam(char *, struct type *, enum typequal);

extern struct type typevoid;
extern struct type typebool;
extern struct type typechar;
extern struct type typei8, typeu8;
extern struct type typei16, typeu16;
extern struct type typei32, typeu32;
extern struct type typei64, typeu64;
extern struct type typef32, typef64;
extern struct type typevalist, typevalistptr;

/* targ */

struct target {
	const char *name;
	struct type *typerune;
	struct type *typelong, *typeulong;
	struct type *typeint, *typeuint;
};

extern struct target *targ;

void targinit(const char *);

/* decl */

struct decl *mkdecl(enum declkind, struct type *, enum typequal, enum linkage);
_Bool decl(struct scope *, struct func *, _Bool);
struct type *typename(struct scope *, enum typequal *);

struct decl *stringdecl(struct expr *);

void emittentativedefns(void);

/* mangle */

char *mangleop(enum tokenkind, struct type *, struct type *);
char *manglegen(enum tokenkind, struct type *);

/* scope */

void scopeinit(void);
struct scope *mkscope(struct scope *);
struct scope *delscope(struct scope *);

void scopeputdecl(struct scope *, const char *, struct decl *);
struct decl *scopegetdecl(struct scope *, const char *, _Bool);

void scopeputtag(struct scope *, const char *, struct type *);
struct type *scopegettag(struct scope *, const char *, _Bool);

extern struct scope filescope;

/* expr */

struct expr *expr(struct scope *);
struct expr *assignexpr(struct scope *);
struct expr *constexpr(struct scope *);
uint64_t intconstexpr(struct scope *, _Bool);
void delexpr(struct expr *);

struct expr *exprconvert(struct expr *, struct type *);
struct expr *exprpromote(struct expr *);

/* eval */

enum evalkind {
	EVALARITH,  /* arithmetic constant expression */
	EVALINIT,   /* initializer constant expression */
};

struct expr *eval(struct expr *, enum evalkind);

/* init */

struct init *mkinit(uint64_t, uint64_t, struct bitfield, struct expr *);
struct init *parseinit(struct scope *, struct type *);

void stmt(struct func *, struct scope *);

/* backend */

struct gotolabel {
	struct value *label;
	_Bool defined;
};

struct switchcases {
	void *root;
	struct value *defaultlabel;
};

struct repr;

struct switchcases *mkswitch(void);
void switchcase(struct switchcases *, uint64_t, struct value *);

struct value *mkblock(char *);

struct value *mkglobal(char *, _Bool);
char *globalname(struct value *);

struct value *mkintconst(struct repr *, uint64_t);
uint64_t intconstvalue(struct value *);

struct func *mkfunc(struct decl *, char *, struct type *, struct scope *);
void delfunc(struct func *);
struct type *functype(struct func *);
void funclabel(struct func *, struct value *);
struct value *funcexpr(struct func *, struct expr *);
void funcjmp(struct func *, struct value *);
void funcjnz(struct func *, struct value *, struct value *, struct value *);
void funcret(struct func *, struct value *);
struct gotolabel *funcgoto(struct func *, char *);
void funcswitch(struct func *, struct value *, struct switchcases *, struct value *);
void funcinit(struct func *, struct decl *, struct init *);

void emitfunc(struct func *, _Bool);
void emitdata(struct decl *,  struct init *);

extern struct repr i8, i16, i32, i64, f32, f64;
