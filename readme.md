## Todos/ideas

### Core semantics
* [ ] logic and/or
* [ ] recursion
* [ ] native functions

### Syntax sugar
* [ ] `a |> f(x, y)` as sugar for `f(a, x, y)`
* [ ] `if b {x} else if b1 {y} else {z}` as sugar for `if b {x} else {if b1 {y} else {z}}`
* [ ] `if b {x}` as sugar for `if b {x} else {nil}`
* [ ] `let x = value \n 0` as sugar for `let x = value; 0`
* [ ] `a + b` as sugar for `intrinsic.add(1, 2)`

### Perf opt
* [ ] TCO
* [ ] constant folding
* [ ] functions inlining

### Extra
* [ ] comments
* [ ] better str parsing
* [ ] function name inference
* [ ] arbitrary infix operators
* [ ] modules
* [ ] repl hot reloading
* [ ] concurrency
* [ ] persistent data structures (list, records, tuples, syms)
* [ ] pattern matching
* [ ] better parsing errors
* [ ] formatter
* [ ] static analysis
  * [ ] unused code
  * [ ] unused vars
* [ ] test suite
* [ ] abstractions
* [ ] better GC
