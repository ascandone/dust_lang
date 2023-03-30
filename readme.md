## Todos/ideas

```
🟢 easy
🟠 medium
🔴 hard

⭐️ todo
```


### Core semantics
* [x] 🟠 logic and/or
* [x] 🟠 recursion
* [ ] 🟠 ⭐ native functions

### Syntax sugar
* [X] 🟢 `a |> f(x, y)` as sugar for `f(a, x, y)`
* [X] 🟢 `if b {x} else if b1 {y} else {z}` as sugar for `if b {x} else {if b1 {y} else {z}}`
* [ ] 🟠 `let x = value \n 0` as sugar for `let x = value; 0`
* [ ] 🟢 `a + b` as sugar for `intrinsic.add(1, 2)` (depends on AST-CST splitting + native functions + fn inlining)
* [ ] 🟢 `if b {x}` as sugar for `if b {x} else {nil}`

### Perf opt
* [ ] 🟠 ⭐ TCO
* [ ] 🟢 constant folding (depends on AST-CST splitting)
* [ ] 🟠 functions inlining
* [ ] 🔴 better GC

### Extra
* [x] 🟢 blank identifiers (prevent _ to bind variables)
* [x] 🟢 comments
* [x] 🟢 function name inference
* [ ] 🟢 arbitrary infix operators (depends on AST-CST splitting)
* [ ] 🟠 static analysis (depends on AST-CST splitting)
  * [ ] unused code
  * [ ] unused vars
* [ ] 🟠 modules
* [ ] 🔴 repl hot reloading (depends on modules)
* [ ] 🔴 concurrency
* [ ] 🔴 better parsing errors (depends on AST-CST splitting)
* [ ] 🔴 formatter (depends on AST-CST splitting)
* [ ] 🟠 persistent data structures (list, records, tuples, syms) 
* [ ] 🔴 pattern matching
* [ ] 🔴 test suite
* [ ] 🟢 better str parsing
* [ ] 🔴 abstractions
