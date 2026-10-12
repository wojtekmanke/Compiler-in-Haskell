# latc: a Latte compiler for x86-64

`latc` compiles Latte, a small statically typed language with Java-like
syntax, to x86-64 assembly for Linux. It is written in Haskell and implements
the core language together with arrays and structs.

Latte was designed for the compiler construction course at the University of
Warsaw; this compiler was written by Wojciech Mańke.

```java
int gcd(int a, int b) {
  while (b != 0) {
    int r = a % b;
    a = b;
    b = r;
  }
  return a;
}

int main() {
  printInt(gcd(readInt(), readInt()));
  return 0;
}
```

```console
$ ./latc_x86_64 gcd.lat
OK
$ echo "1071 462" | ./gcd
21
```

## Building

You need GHC and cabal (tested with GHC 9.10 and cabal 3.14), a C compiler
(tested with GCC 12) and GNU make. The compiler itself builds anywhere, but
the programs it produces are for x86-64 Linux.

```console
$ cabal update    # once, so that cabal can fetch the alex and happy build tools
$ make            # builds ./latc and runtime/runtime.o
$ make test       # runs the test suite
```

On other systems, such as a Mac with Apple silicon, the same commands work in
a container:

```console
$ docker run --rm --platform linux/amd64 -v "$PWD":/src -w /src \
    haskell:9.10.3-slim-bookworm sh -c 'cabal update && make test'
```

## Usage

```console
$ ./latc_x86_64 path/to/program.lat
```

writes the assembly to `path/to/program.s` and the executable to
`path/to/program`. The first line printed to the standard error is `OK`, or
`ERROR` followed by a description of the problem, in which case the exit
status is 1:

```console
$ ./latc_x86_64 bad.lat
ERROR
bad.lat:2:11: error: expected an expression of type `int`, found `string`
```

`latc_x86_64` is a thin driver. It runs the compiler proper, `latc`, which
only translates Latte to assembly (`./latc program.lat -o program.s`), and
then has the C compiler assemble the result and link it with the runtime
library. The C compiler is `cc`, or `$CC` if that is set, for example to a
cross-compiler.

## The language

A program is a list of functions and classes, in any order. Execution starts
in `int main()`, whose result is the exit status of the program.

| | |
|---|---|
| Types | `int` (32 bits, arithmetic wraps around), `boolean`, `string`, `void`, arrays `T[]`, classes |
| Statements | declarations, assignment, `++` and `--`, `if`/`else`, `while`, `for (T x : array)`, `return`, blocks |
| Expressions | `+ - * / %`, comparisons, `&& \|\| !` with short-circuit evaluation, calls, `new C`, `new T[n]`, `a[i]`, `a.length`, `object.field`, `(C)null` |
| Built-in functions | `printInt`, `printString`, `readInt`, `readString`, `error` |

Classes are structs: they have fields, but no methods and no inheritance.
Objects and arrays are handled by reference.

The language definition leaves some details open. This compiler settles them
as follows.

- **Names.** A variable can be declared only once in a block, and may hide a
  variable of an enclosing block. Unlike in plain Latte, and as in Java, it
  cannot hide a parameter of its function. Variables, functions and classes
  share one namespace, so a variable cannot be named after a function or a
  class.
- **Default values.** Variables, fields and array elements that are not
  initialised explicitly are `0`, `false`, the empty string or `null`.
- **Strings** are immutable and are compared by content.
- **Evaluation order** is left to right. In an assignment, the right-hand side
  is evaluated before the location that is assigned to.
- **`for` loops** evaluate the array once. The loop variable is a copy of the
  current element.
- **Returning.** Type checking sees the program as written, so both branches
  of `if (true) return 1; else return "one";` are checked and the program is
  rejected. Whether a function always returns a value is decided after
  constant folding, so `int f() { if (true) return 1; }` is accepted. Control
  never gets past `while (true)`, since there is no `break`, or past a call of
  `error()`. As in Java, a `void` function cannot return an expression, not
  even a call of another `void` function.
- **Input.** `readInt` skips white space, reads a number and, if nothing else
  is left on its line, the line break. `readString` reads a whole line; at
  the end of input it returns the empty string.
- **Run-time errors.** `error()` prints `runtime error` and ends the program
  with exit status 1. So do running out of memory, allocating an array of
  negative length, and `readInt` when the input is not a number that fits in
  an `int`. Division by zero, and the division of the smallest `int` by -1,
  are left to the processor, which raises `SIGFPE`. Null references and array
  indices are not checked. Output is written line by line, so nothing printed
  before a crash is lost.

## How it works

```
          parser                  type checker
  source ────────► syntax tree ────────────────► core program
                                                      │ constant folding
                                                      │ return checking
                                                      ▼
  assembly ◄──────────────── quadruples ◄─────── core program
             code generator   (+ liveness)
```

`Latte.Compiler` runs this pipeline as a pure function from source text to
assembly.

### Frontend

The lexer, the parser and the syntax tree are generated by BNFC from
[`grammar/Latte.cf`](grammar/Latte.cf); every node of the tree carries its
position in the source.

[`Latte.Frontend.TypeCheck`](src/Latte/Frontend/TypeCheck.hs) resolves names
and checks types. It does not just accept or reject the program: it
translates the syntax tree into the core language of
[`Latte.Core`](src/Latte/Core.hs), in which nothing is left to look up or
infer. Variables are numbered, so scopes no longer matter; a field access
carries the field's offset and type; `+` is either an addition or a
concatenation, and `==` either a comparison of values or of strings. The
later passes therefore cannot disagree with the checker about what the
program means, and never have to handle a case that it has ruled out.

[`Latte.Frontend.ConstFold`](src/Latte/Frontend/ConstFold.hs) evaluates
constant expressions and removes branches and loops whose condition is a
constant. Folding gives exactly the results of the generated code: arithmetic
wraps around and division truncates towards zero. A division that would trap
at run time is left alone.

[`Latte.Frontend`](src/Latte/Frontend.hs) then checks that every function
that has to return a value does so on every path.

### Intermediate representation

[`Latte.Quadruples`](src/Latte/Quadruples.hs) defines a conventional
three-address code. A function is a control-flow graph of basic blocks; each
block is a sequence of quadruples followed by a single terminator, which is a
jump, a two-way branch on a comparison, or a return.

[`Latte.Quadruples.Generate`](src/Latte/Quadruples/Generate.hs) translates the
core program. Conditions become jumping code: instead of computing a boolean
and testing it, `a < b && c < d` turns directly into two conditional branches,
which is also what makes `&&` and `||` short-circuit. Blocks that cannot be
reached are dropped.

[`Latte.Quadruples.Liveness`](src/Latte/Quadruples/Liveness.hs) computes, by
the standard backward data-flow analysis over the whole function, which
variables are live after every instruction.

### Code generation

[`Latte.Backend.X86_64`](src/Latte/Backend/X86_64.hs) allocates registers
locally, one basic block at a time. While translating a block it tracks which
variables each register holds and whether the stack slot of each variable is
up to date. Values are loaded on first use and stored back only if they are
still live when their register is needed for something else, at a call, or at
the end of the block. When all registers are taken, the one that is cheapest
to free is chosen, so the output is deterministic. A copy `x = y` generates no
code at all: the two variables simply share a register until one of them
changes.

Instructions are values of a small data type,
[`Latte.Backend.Asm`](src/Latte/Backend/Asm.hs), and are turned into text only
at the very end.

For the `gcd` function above the compiler produces:

```asm
latte_gcd:
	pushq	%rbp
	movq	%rsp, %rbp
	jmp	.L2
.L1:
	movl	16(%rbp), %eax
	cltd
	idivl	24(%rbp)
	movl	24(%rbp), %eax
	movl	%eax, 16(%rbp)
	movl	%edx, 24(%rbp)
.L2:
	movl	24(%rbp), %eax
	cmpl	$0, %eax
	jne	.L1
	movl	16(%rbp), %eax
	leave
	ret
```

**Calling convention.** Latte functions take their arguments on the stack,
pushed from the last to the first and popped by the caller, and return their
result in `%rax`. Only caller-saved registers are ever allocated, so a
function preserves all callee-saved registers without any work, and `%rsp` is
kept aligned to 16 bytes at every call. The runtime library is called
according to the System V ABI.

**Data** ([`Latte.Layout`](src/Latte/Layout.hs)). Integers and booleans take
32 bits, references 64. An object is a block of fields laid out in declaration
order, each aligned to its size. An array is a pointer to its length, which is
followed by the elements. A string is a pointer to NUL-terminated bytes;
string literals live in `.rodata`.

**Symbols.** A function `f` is emitted as `latte_f`, which keeps Latte
functions with names like `malloc` or `exit` from clashing with the C library.
The functions of the runtime library are named `lrt_*`.

### Runtime library

[`runtime/runtime.c`](runtime/runtime.c) implements the built-in functions,
string concatenation and comparison, and allocation. It also provides the C
`main`, which calls the program's `latte_main`.

## Testing

`make test` compiles and runs every program under `tests/`:

- programs in `tests/good` and `tests/extensions/{arrays1,struct}` must
  compile and, given the `.input` file next to them if there is one, print
  exactly their `.output` file and finish with the exit status in their
  `.status` file, or with 0 if they have none;
- programs in `tests/bad` must be rejected. If the first line of one is a
  comment `// error: TEXT`, the error message must contain `TEXT`.

`tests/good/core*`, `tests/bad/bad*` and `tests/extensions` are the test suite
distributed with the course. The remaining tests each target a specific part
of the compiler, among them register pressure, division, scoping,
short-circuit evaluation, string handling, input, run-time errors, and the
messages of the type checker.

`tests/run.sh FILE.lat...` runs individual tests.

## Project layout

```
app/Main.hs               command-line interface of latc
src/Latte/
  Compiler.hs             the pipeline: source text to assembly
  Error.hs                compile errors and their positions
  Types.hs                types of values and functions, built-in functions
  Layout.hs               how values are represented in memory
  Core.hs                 the core language: the program, resolved and typed
  Frontend.hs             frontend entry point, return checking
  Frontend/TypeCheck.hs   name resolution, type checking, translation to core
  Frontend/ConstFold.hs   constant folding
  Quadruples.hs           the intermediate representation
  Quadruples/Generate.hs  core to quadruples
  Quadruples/Liveness.hs  liveness analysis
  Backend/X86_64.hs       register allocation and code generation
  Backend/Asm.hs          x86-64 instructions and their syntax
  Abs.hs, Lex.x, Par.y    generated by BNFC (make grammar)
grammar/Latte.cf          the grammar
runtime/runtime.c         the runtime library
latc_x86_64               driver: compile, assemble and link
tests/                    test programs and tests/run.sh
```

## Limitations

- Classes have no methods or inheritance, so `tests/extensions/objects1` and
  `objects2` from the course's test suite are not supported.
- `null` can only be written for a class, as `(C)null`. An array variable
  that was never assigned is null as well, but there is no literal to compare
  it with.
- Memory is never freed; there is no garbage collector.
- Register allocation is local to a basic block: every live variable is
  written to the stack at the end of a block and read back in the next one.
  Allocating registers for whole functions is the natural next step.
- Apart from constant folding, there are no optimisation passes.
