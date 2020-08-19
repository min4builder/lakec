# Language implemented by `lakec`

This is *not* the official spec. This is an informal spec as implemented
by `lakec`. Currently, there is no official spec, so this will have to
suffice. This also doesn't include planned additions that haven't been
implemented yet. See README.md and GOAL.md for those.

Note that the language is still under development, in particular the
syntax may be changed. Also, the documentation is not complete and refers
to the reader's knowledge of C at times.

## Declarations

On the top level:

    /* everything is static (private) by default */
    [pub] name type [= expr];
    [pub] name (arg type) type { statements; }

    /* if pub or static is not specified, it is pub unless specified later */
    [pub|static] name (arg type) type;

Elsewhere:

    (pub|static|auto) name type [= expr];
    (pub|static) name (arg type) type;
    /* no nested function definitions */

Types may be ommited if there is a value (and this is idiomatic):

    /* top level */
    x = 35;
    y = "Strings also work";
    z = [struct(x, y int)](3, 4);
    /* function context */
    auto x = 35;
    auto y = "Strings also work";
    auto z = [struct(x, y int)](3, 4);

Multiple variables with the same type may be declared at once:

    x, y, z = 1, 2, 3;
    f(a1, a2 int) int {
        return a1 + a2;
    }

Operator overloading:

    .+(a, b complex) {
        return [complex](a.real + b.real, a.imag + b.imag);
    }

These act as normal function calls.

Overloadable operators are: `-`, `+`, `~`, `&`, `|`, `^`, `&&`, `||`, `+`, `-`,
`*`, `/`, `%`, `<<`, `>>`, `==`, `!=`, `<`, `>`, `<=`, `>=`, `++` and `--`.
They have the same ABI as in C++ (except `+` and `-`, due to a bug).

Overloads for assignment operators (`<op>=`) are generated from their
respective binary operators. Postfix `++` and `--` are generated from
their prefix versions.

Other things:

    noreturn f() void; /* f cannot return */
    g() void asm("__internal_g"); /* g's name in assembly (and other languages) */
    static_assert(1, "1 is not true"); /* compile-time assert */

## Types

Builtin types:

- `u8` - unsigned 8-bit integer
- `i8` - signed 8-bit integer
- `u16` - unsigned 16-bit integer
- `i16` - signed 16-bit integer
- `u32` - unsigned 32-bit integer
- `i32` - signed 32-bit integer
- `u64` - unsigned 64-bit integer
- `i64` - signed 64-bit integer
- `f32` - 32-bit IEEE floating point number
- `f64` - 64-bit IEEE floating point number
- `char` - UTF-8 byte
- `rune` - UTF-32 code point
- `bool` - boolean integer
- `uint`/`int` - unsigned and signed "natural" integers, whatever would
  be `int` for C
- `ulong`/`long` - unsigned and signed pointer-sized integers
- `__builtin_va_list` - C's `va_list` type

Composite types:

    *T /* pointer; usually treated as reference but can do as in C */
    []T /* array without a bound; becomes a pointer as function argument */
    [n]T /* bounded array; passed by reference but keeps size as function argument */
    struct(x, y T, z U) /* struct */
    union(x T, y U) /* tagless union (be careful with these) */
    enum(A, B, C = 3) /* int or uint with prebuilt constants */
    (_, _ T, _ U) V /* function type */

Struct and union tags:

    struct a(
       ...
    );
    x = [a](blah);

Tagless structs and unions are structurally typed:

    struct a(x, y int);
    struct b(x, y int);
    typedef c struct(a, b int);
    /* c is compatible between a and b, but a and b are not compatible
       between each other. Note the field names are different */

In C, `const` can be used to indicate that something will not be changed.
In Lake, `mut` must be used to indicate that something *will* be changed.
This means you don't have to smear `const` markers all over the codebase,
and is in general more useful, but it does make `mut` mandatory.

    *mut int /* pointer to mutable int */
    mut *[]int /* mutable pointer to (constant) array of ints */
    mut *mut []int /* mutable pointer to mutable array of ints */
    mut []int == []mut int /* use the first, please */

`volatile` also works as in C.

## Statements

Currently, mostly the same as C, except for declarations and:

Switch:

    switch (value) {
    case 1:
        /* break is implicit; no fallthrough here */
    case 2:
        /* continue can be used to change that */
        continue 1;
    case 3:
        value = 2;
        /* can be a dynamic expression */
        continue value;
    else:
        /* default is else */
    }

## Expressions

Currently, mostly the same as C, except:

    sizeof[int] == sizeof(3) /* notice the brackets */
    expr->[type] /* cast syntax */
    expr.property /* also works with pointers to structs and unions */
    [[]int](1, 2, 3) /* struct and array literals */
    if(cond) 1 else 2 /* instead of ternary operator */

## Built-in functions and macros

- `alloca(size ulong) *mut void`: Allocate memory on the stack.
- `offsetof(type, member) ulong`: Return the offset of a member in a
  struct or union.
- `__builtin_constant_p(expr) bool`: Test whether the argument is a
  constant expression.
- `__builtin_inff() f64`: positive infinity value.
- `__builtin_nanf("") f64`: quiet NaN value.
- `__builtin_types_compatible_p(t, u) bool`: Test whether the two types
  are compatible.
- `__builtin_va_arg(list *__builtin_va_list, type) type`: Take the next
  argument from the vararg list.
- `__builtin_va_copy(a *mut __builtin_va_list, b *__builtin_va_list) void`:
  Copy a `va_list`.
- `__builtin_va_end(list *mut __builtin_va_list) void`: End the reading of
  varargs.
- `__builtin_va_start(list *mut __builtin_va_list) void`: Start the reading
  of varargs.

