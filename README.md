# `saf`

[![build](https://github.com/paasim/saf/workflows/build/badge.svg)](https://github.com/paasim/saf/actions)

`saf` (`standard ascii functions`) is a small programming language.

## usage

There are two binaries, an interpreter and a compiler. They are used like so:

```bash
# build the compiler and the interpreter with cargo,
# in some cases the interpreter provides less obscure error messages
# prebuilt versions also exist as releases
cargo build -r
[ ! -L safi ] && ln -s ./target/release/interpreter safi
[ ! -L safc ] && ln -s ./target/release/compiler safc

./safi # start repl
./safi -f example.saf # interpret a file

./safc # start jit-compiled repl
./safc -c example.saf # compiles the file into example.safc
./safc -f example.safc # runs the compiled file
```

## language

`saf` has booleans, (signed) integers, strings, linked lists and functions. It works like so:

```saf
> x = "test";
> x;
"test"
>
> z = 5;
> plus = fn(x, y) x + y;
> plus(z, 2);
7
> plus_z = plus(z);
> plus_z(2);
7
> plus(z)(2);
7
>
> twice = fn(x, f) { f(f(x)) };
> twice(7, plus_z);
17
>
> fib = fn(n) n < 3 ? 1 : fib(n-1) + fib(n-2);
> fib(10);
55
>
> v = [1, 2, 13];
> v;
[1, 2, 13]
> -v;
13
> <v;
[1, 2]
> v + 5 + 7;
[1, 2, 13, 5, 7]
>
> mk_arr = fn(i) i == 0 ? [] : mk_arr(i-1) + (i - 1);
> arr = mk_arr(5);
> arr;
[0, 1, 2, 3, 4]
>
> map = fn(v, f) !v ? [] : map(<v, f) + f(-v);
> map(arr, fn(x) x * 2);
[0, 2, 4, 6, 8]
>
> reduce = fn(v, f, acc) !v ? acc : reduce(<v, f, f(acc, -v));
> reduce(arr, fn(acc, x) acc + x, 0);
10
```

Mostly it is like any other untyped programming language except for few special operators:
- `!`: unary prefix operator that checks if a list is empty
- `<`: unary prefix operator that returns all the elements of the list except for the last one
- `-`: unary prefix operator that returns the last element of the list
- `+`: binary midfix operator that appends an element to a list
