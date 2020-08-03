The P compiler is based on the `cproc` C compiler, since both languages
are rather similar.

The compiler and language are still under development, and the syntax
in particular is in flux. My personal notes on it can be read in `GOAL.md`.

`cproc` is a [C11] compiler using [QBE] as a backend. It is released
under the [ISC] license.

It was inspired by several other small C compilers including [8cc],
[c], [lacc], and [scc].

## Requirements

The compiler itself is written in standard C11 and can be built with
any conforming C11 compiler.

The POSIX driver depends on POSIX.1-2008 interfaces, and the `Makefile`
requires a POSIX-compatible make(1).

At runtime, you will need QBE, an assembler, and a linker for the
target system. Currently, Michael Forney's personal [QBE branch] is
recommended, since it may address some issues that have not yet made
it upstream.

## Supported targets

All architectures supported by QBE should work (currently x86\_64 and
aarch64).

## Building

Run `./configure` to create a `config.h` and `config.mk` appropriate for
your system. If your system is not supported by the configure script,
you can create these files manually. `config.h` should define several
string arrays (`static char *[]`):

- **`startfiles`**: Objects to pass to the linker at the beginning of
  the link command.
- **`endfiles`**: Objects to pass to the linker at the end of the link
  command (including libc).
- **`preprocesscmd`**: The preprocessor command, and any necessary flags
  for the target system.
- **`codegencmd`**: The QBE command, and possibly explicit target flags.
- **`assemblecmd`**: The assembler command.
- **`linkcmd`**: The linker command.

You may also want to customize your environment or `config.mk` with the
appropriate `CC`, `CFLAGS` and `LDFLAGS`.

If you don't have QBE installed, you can build it from the included
submodule (NOTE: BSD users will need to use gmake here), then add it to
your PATH so that the driver will be able to run it.

	make qbe
	PATH=$PWD/qbe/obj:$PATH

Once this is done, you can build with

	make

## Automated tests

Run `make check` to run the automated regression suite. It has been
updated to run in P, and reading it is probably the best way to get
a feel for what the language is about, until proper documentation is
in place. It is available under `test/*.c`.

[QBE]: https://c9x.me/compile/
[C11]: http://port70.net/~nsz/c/c11/n1570.html
[ISC]: https://git.sr.ht/~mcf/cproc/blob/master/LICENSE
[8cc]: https://github.com/rui314/8cc
[c]: https://github.com/andrewchambers/c
[lacc]: https://github.com/larmel/lacc
[scc]: http://www.simple-cc.org/
[QBE branch]: https://git.sr.ht/~mcf/qbe
