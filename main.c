#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include "util.h"
#include "arg.h"
#include "cc.h"

static noreturn void
usage(void)
{
	fprintf(stderr, "usage: %s [-E] [-t target] [-o out] [input]\n", argv0);
	exit(2);
}

int
main(int argc, char *argv[])
{
	bool pponly = false;
	char *output = NULL, *target = NULL;

	argv0 = progname(argv[0], "cproc-qbe");
	ARGBEGIN {
	case 'E':
		pponly = true;
		break;
	case 'I':
		ppdir(EARGF(usage()));
		break;
	case 't':
		target = EARGF(usage());
		break;
	case 'o':
		output = EARGF(usage());
		break;
	default:
		usage();
	} ARGEND

	targinit(target);

	if (output && !freopen(output, "w", stdout))
		fatal("open %s:", output);

	if (argc) {
		while (argc--)
			scanfrom(argv[argc], NULL);
		scanopen();
	} else {
		scanfrom("<stdin>", stdin);
	}

	ppinit();
	if (pponly) {
		ppflags |= PPNEWLINE;
		while (tok.kind != TEOF) {
			tokenprint(&tok);
			next();
		}
	} else {
		scopeinit();
		while (tok.kind != TEOF) {
			if (!decl(&filescope, NULL, false))
				error(&tok.loc, "expected declaration or function definition");
		}
		emittentativedefns();
	}

	fflush(stdout);
	if (ferror(stdout))
		fatal("write failed");
	return 0;
}
