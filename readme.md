# Dust lang

> A language that actually fits in your head, with lots of sugar in it

Dust is a small functional (everything is an expression), untyped language  with a minimal core AST.

```
let fact = fn n {
    if n == 0 || n == 1 {
        1
    } else {
        fact(n + 1)
    }
}
```

 

### Roadmap
Dust is still missing basic functionality, such as a variety of data structures, nicer error messages,
or developer tools such as a formatter, a test runner, or even a LSP.
I'm tracking the missing features in this [notion page](https://ascandone.notion.site/7534c8c846414e0c9ad2906540af5bc4?v=1685e0defdde4b768a2fe071e4ffd056).
