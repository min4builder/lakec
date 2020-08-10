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
- simple type inference (auto DONE, missing const)

      auto x = 65;
      auto y = x->[f32];
      static seventyhundred = 7000.0f;

- type-safe & lw generics (half-done, no real syntax)

      #define Vec(T) struct(len, cap, size ulong, vec *T)
      a Vec(int) = (0, 0, sizeof[int], 0);
      #define vecnew(T) (vecnew_(sizeof[T])->[Vec(T)])
      vecnew_(size int) Vec(void) {
          return [Vec(void)](0, 0, size, 0);
      }

- operator overloading (working, with C++ mangling, nice syntax)

      .<<(f *FILE, i int) void
      {
          fprintf(f, "%d", i);
      }
      stdout << 32;

- new decl syntax (postfix and cast DONE, missing init, compound literals, etc.)

      a *[32]int = &[[32]int](0);
      struct a (x : 3, _ : 0, y : -4, a, b int);
      e struct a = (2, 4);
      b = a->[*int];
      c = sizeof(b);
      d = sizeof[int];

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

- easier res management (copy, move & drop)
- module system
- good language extension system (decls, exprs, attrs)
