## Todos/ideas

### Core semantics
* [x] MEDIUM logic and/or
* [ ] MEDIUM recursion
* [ ] MEDIUM native functions

### Syntax sugar
* [X] EASY `a |> f(x, y)` as sugar for `f(a, x, y)`
* [X] EASY `if b {x} else if b1 {y} else {z}` as sugar for `if b {x} else {if b1 {y} else {z}}`
* [ ] ?MEDIUM `let x = value \n 0` as sugar for `let x = value; 0`
* [ ] EASY `a + b` as sugar for `intrinsic.add(1, 2)` (depends on AST-CST splitting)
* [ ] EASY `if b {x}` as sugar for `if b {x} else {nil}`

### Perf opt
* [ ] MEDIUM TCO (depends on recursive bindings)
* [ ] EASY constant folding (depends on AST-CST splitting)
* [ ] MEDIUM functions inlining
* [ ] HARD better GC

### Extra
* [x] EASY comments
* [ ] EASY function name inference
* [ ] EASY arbitrary infix operators (depends on AST-CST splitting)
* [ ] MEDIUM static analysis (depends on AST-CST splitting)
  * [ ] unused code
  * [ ] unused vars
* [ ] MEDIUM modules
* [ ] HARD repl hot reloading (depends on modules)
* [ ] HARD concurrency
* [ ] HARD better parsing errors (depends on AST-CST splitting)
* [ ] HARD formatter
* [ ] MEDIUM persistent data structures (list, records, tuples, syms) 
* [ ] HARD pattern matching
* [ ] HARD test suite
* [ ] EASY better str parsing
* [ ] HARD abstractions
