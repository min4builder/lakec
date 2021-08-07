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

Parametric functions and variables:

    f[type T](n *T) *T { return n; }
    x [type T]*(*T)void = &f;

Since the type isn't known, you can't do anything with it directly.
It is possible to manipulate it behind pointers, however.

When a parametric variable or function is referenced,
you must specify which types it's going to be instanced with:

    f[(*int)void](x[int])

There is higher-level and higher-kinded polymorphism.

Operator overloading:

    .+(a, b complex) complex {
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

    f() noreturn; /* f doesn't return */
    #[name "__internal_g"] g() void; /* g's name in assembly (and C) */
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
    [type a, b, c]T /* parametric type */

Struct and union tags:

    type a struct(
       ...
    );
    x = [a](blah);

Structs and unions are structurally typed:

    type a struct(x, y int);
    type b struct(x, y int);
    type c struct(a, b int);
    /* a, b and c are compatible with each other.
       Notice the field names are different */

In C, `const` can be used to indicate that something will not be changed.
In Lake, `mut` must be used to indicate that something *may* be changed.
This means you don't have to smear `const` markers all over the codebase,
and is in general more useful, but it does make `mut` mandatory.

    *mut int /* pointer to mutable int */
    mut *[]int /* mutable pointer to (constant) array of ints */
    mut *mut []int /* mutable pointer to mutable array of ints */
    mut []int == []mut int /* the first one is idiomatic */

`volatile` and `restrict` also work as in C,
but are called `#volatile` and `#restrict` respectively.

Parametric types can be applied with `[]` or, if it's only one argument, a space:

    type list [type T]struct(next *list T, elem T);
    list T == list[T] /* the first one is idiomatic */

`#nocopy` can be used to say a value must be used AT MOST once.
This gives it "move semantics" more or less as in Rust:

    auto x #nocopy *int = malloc(sizeof[int]);
    *x = 1;
    pub f(_ *int) void;
    f(x); /* casting to a copiable reference works */
    pub free(_ #nocopy *void) void;
    free(x);
    /* now x can't be used anymore */
    /* free(x); */ /* will error */

`#nodrop` can be used to say a value must be used AT LEAST once.
This prevents it from being thrown away without proper handling:

    free(_ #nodrop *void) void;
    auto x #nodrop *int = malloc(sizeof(*x));
    /* if you forget to free(x), there will be an error */

Both `#nocopy` and `#nodrop` can be combined to make a linear type of sorts.
The tracking is *not* perfect and it might miss something.
There should be no false positives, at least.

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
    0 else 32 /* return first if nonzero, second otherwise */
    /* break, continue, goto and return are expressions */

Stopgap solutions (that will be replaced in the future):

    e->auto x (...) /* e is bound to x inside ... */

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
- `__builtin_va_arg(list *mut __builtin_va_list, type) type`: Take the next
  argument from the vararg list.
- `__builtin_va_copy(a *mut __builtin_va_list, b *__builtin_va_list) void`:
  Copy a `va_list`.
- `__builtin_va_start() *mut __builtin_va_list`: Start the reading of varargs.

