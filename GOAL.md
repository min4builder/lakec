- Simplicity is good
- For low level, procedural > everything else
- C has no modularity (easy to work around)
- C has no genericity (sometimes easy to work around)
- C has no way to check that MM is right (hard in general, see Rust)
- C has no operator overloading (easy to do, C doesn't)
- C has a rather messy standard library (except in Plan 9)
- C has a rather messy threading system (except in Plan 9)
- varargs are weird
- the declaration syntax is weird
- the preprocessor is weird

C, with:
- simple type inference

      auto x = 65;
      x = 66; /* error; constant by default */
      auto y mut = x->[f32];
      y = 32.0; /* ok */
      static seventyhundred = 7000.0f;

- new types

      char - character (u8)
      rune - wide char (u32)
      bool - boolean (1 bit)
      u8/i8 - 8 bits
      u16/i16 - 16 bits
      u32/i32 - 32 bits
      u64/i64 - 64 bits
      f32 - float
      f64 - double
      uint/int - natural type (unsigned/int)
      ulong/long - pointer type (size_t/ssize_t/uintptr_t/intptr_t/ptrdiff_t)

- operator overloading (working, with C++ mangling, nice syntax)

      .<<(f *FILE, i int) void
      {
          fprintf(f, "%d", i);
      }
      stdout << 32;

- module system (static by default, basic import DONE, missing namespacing (?))

      /* on header.ph */
      typedef t thing;
      other() int;

      import "header.ph";
      /* static by default */
      g() int {
          return f();
      }
      /* but declarations don't count */
      f() int;
      pub f() int {
          return other();
      }

- new decl syntax (postfix and cast DONE, missing init, compound literals, etc.)

      a *[32]int = &[[32]int](0);
      struct a (x : 3, _ : 0, y : -4, a, b int);
      e struct a = (2, 4);
      b = a->[*int];
      c = sizeof(b);
      d = sizeof[int];

- better preprocessor (better call syntax DONE, definition not very good)

      define a(b) {
          b
      }
      define c[T] = *T;
      a(x c int);

- easier resource management

      define own[T] = #nocopy #nodrop T;
      define alloc[T] = (alloc_(sizeof[T])->[own *T]);
      alloc_(_ ulong) own *void;
      free(_ own *void) void;

      auto x = alloc[int];
      auto y = x;
      free(y); /* if you remove this line, #nodrop will error */
      /* free(x); */ /* if you uncomment this line, #nocopy will error */

- type-safe & lw generics (WIP, no real syntax)

      define Vec[T] = struct(len, cap, size ulong, vec *T);
      a Vec int = (0, 0, sizeof[int], 0);
      define vecnew[T] {
          (vecnew_(sizeof[T])->[Vec T])
      }
      vecnew_(size ulong) Vec void {
          return [Vec void](0, 0, size, 0);
      }

- easier error handling (switch/continue DONE, result)

      switch (0) {
      case 0:
          e = callthatmayerror();
          if (e) continue e;
          e = othercallthatmayerror();
          if (e) continue e;
      case EFOO:
          /* handle */
      case EBAR:
          /* handle */
      else:
          puts("I don't know that error");
      }

- good language extension system (macros, decls, attrs)
- good standard library (plan 9-inspired, with threading, graphics, etc.)

