# Lake is C but better

Lake is a new programming language, meant to replace C for the author
personally, as a kernel- and game-development tool, and also to be
useful to other people in the process.

    /* calling the C function because the standard library is not ready */
    puts(_ []char) int;

    pub main() int
    {
        puts("Hello, world!")
        return 0;
    }

Idea:

- Like C, simple and small
- Like C, fast compilation
- Like C, doesn't get in your way
- Like C, doesn't have too many strong opinions
- Like C, low- to mid-level
- Like C, everything is explicit
- Unlike C, easy to write safe code in (but like C, it's not mandatory)
- Unlike C, well-specified conventions
- Unlike C, extra convenience features (simple type inference, operator
  overloading, generics etc.)
- Easy to mix Lake and C in the same project
- Really, just "C, but better"

Features:

- Procedural (best for given problem domains)
- Operator overloading (useful mainly for games)
- Simple type inference, some structural typing and other convenience features
- Postfix type syntax (similar to Go but a bit shorter)
- Macros (for language extension only)
- Explicit memory management (but (unimplemented) the compiler checks
  that you didn't make any obvious mistake)
- Flexible memory management (stack allocation, (unimplemented, but not
  too far away) reference counting, arenas, garbage collection etc.)
- Well-specified and nice type names (u8/i16 type, plus int for a machine
  integer and long for a pointer integer)
- Full-blown pointers, with pointer arithmetic and no need for `unsafe`
  blocks
- `goto` and other unsafe things
- (WIP) Good standard library ([Plan 9 libc]-inspired)

Future features:

- Generics (partially designed, can do some of it with macros)
- Easier error management (return values, with a twist!)
- Checked memory management (like Rust, but less strict; mostly to catch
  forgetting to `drop()` or duplicating ownership of a pointer)
- Proper modularity (with headers, but no global namespace pollution)
- Standard build system, to avoid proliferation of Make, autoconf, CMake,
  Meson, SCons, Waf etc.

## Why?

The rationale behind it is that C is a pretty good language for what it
was designed for (kernel development) and stood the test of time and
abuse by many people well enough to still be widely used 50 years later.
However, it's an old language, and wasn't designed with some of the
hindsight and resources we have now. While it's still a pretty good
language for kernel development, it would be nice to have some of the
convenience that higher-level languages give without losing the
explicitness and control that C gives. Lake is supposed to fill that gap.

## Why not use something else?

C has a few advantages I think are essential to the language. Everything
is explicit; no code runs without at least one character indicating it.
Also, separate compilation is trivial (though its benefits can easily be
defeated by a bad header design) and the type system is flexible, while
the entire language remains pretty simple and easy to implement from scratch.

It does have plenty of issues though; the declaration syntax is weird,
pointer decay is not ideal, `const` is completely useless, headers are
easy to get wrong, and varargs, the preprocessor and standard library are
all messy and quickly hacked-together solutions that aren't very good.

Let's see how other systems languages fare as "C replacements":

- [C++] has all of C's problems, and adds a few of its own, due to being
  very large. It adds many features for OOP, functional-like programming,
  that aren't really needed on these problem domains, but bloat the
  language and compilation time considerably, and mean every library has
  its own way of doing things, which makes it difficult to keep a
  consistent coding style.

- [D] has a nice standard library, and adds OOP and other stuff too.
  However, the standard library relies pretty heavily on garbage
  collection and, while you can turn it off, the library becomes
  useless then. This is not too bad for kernels, which don't use it
  anyway, but it is really annoying for games, which generally need to
  be pretty aware of memory and resource usage. Also, several of its
  features end up going to waste on these problem domains.

- [Rust] attempts to make a "safe" systems programming language. The
  result, while theoretically and technically impressive, is very
  inconvenient to use, often whining about perfectly safe code. Even
  though you can get used to it, some of its stricter safety rules end
  up making the programmer rely too much on compiler optimisations (I've
  had a program crash when it ran without optimisation, out of memory
  exhaustion, even though it was perfectly correct). It doesn't know
  whether it wants to be a high- or low-level language, and uses
  functional idioms for low level things, obscuring what is the real
  resource usage of the program, while forcing the programmer to
  constantly worry about memory safety.

- [Go] doesn't even qualify; it has a garbage collector that can't be
  turned off (which is bad for kernels). While it is pretty speedy,
  and the language overall has some interesting design decisions
  (and was a major inspiraton for Lake), it has very strong opinions
  on some subjects, that some (and in particular, the author) might not
  agree with. Also, you aren't going to see kernels written in Go so
  soon; that's not what the language was designed for.

- [V] is pretty similar to Go, except for the lack of a garbage collector;
  it also has strong opinions, just different ones.

This is not meant to say these languages are bad, only why not simply
using one of these: they are not really good matches for the uses the
author had in mind. All of them were inspirations for Lake at some point.

## Why "Lake"?

Because it is like C, but less salty (say that out aloud, if you didn't
get it). Also, C++ and D were already taken, and P (next letter in [BCPL])
was taken by a completely unrelated language. Lake seems to be vacant at
this point (hopefully).

## `lakec`

The Lake compiler is based on the `cproc` C compiler, since both languages
are rather similar.

The (WIP) documentation is available under doc/.

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
updated to run in Lake, and reading it is probably the best way to get
a feel for what the language is about, until proper documentation is
in place. It is available under `test/*.lk`.

[Plan 9 libc]: https://9p.io/magic/man2html/2/0intro
[C++]: https://en.wikipedia.org/wiki/C%2B%2B
[D]: https://dlang.org/
[Rust]: https://www.rust-lang.org/
[Go]: https://golang.org/
[V]: https://vlang.io/
[BCPL]: https://en.wikipedia.org/wiki/BCPL
[QBE]: https://c9x.me/compile/
[C11]: http://port70.net/~nsz/c/c11/n1570.html
[ISC]: https://git.sr.ht/~mcf/cproc/blob/master/LICENSE
[8cc]: https://github.com/rui314/8cc
[c]: https://github.com/andrewchambers/c
[lacc]: https://github.com/larmel/lacc
[scc]: http://www.simple-cc.org/
[QBE branch]: https://git.sr.ht/~mcf/qbe
