# Dust lang

> A language that actually fits in your head, with lots of sugar in it

Dust is a small functional (everything is an expression), untyped language  with a minimal core AST.

```
let fizz_buzz = fn n {
  match #(n % 3, n % 5) {
    #(0, 0) => "FizzBuzz",
    #(0, _) => "Fizz",
    #(_, 0) => "Buzz",
    _ => String.show(n),
  }
}
```

### Language tour
You can find a complete guide of Dust language [here](https://ascandone.github.io/dust-lang-book/)

TL;DR: [language cheatsheet](https://ascandone.github.io/dust-lang-book/cheatsheet.html)

### Try Online
You can try Dust without installing it in the [online playground](https://dust-lang-playground.vercel.app/)

### Install locally
> For now the CLI has to be built locally using `cargo build --release`

A CLI is included to create and run Dust projects
```
Usage: dust <command> [<args>]

Dust lang cli

Options:
  --help            display usage information

Commands:
  run               Run a dust script
  init              Initialize a Dust project
  repl              Run the Dust repl
  fmt               Format a Dust script
```

 

### Roadmap
Dust is still missing basic functionality, such as a variety of data structures, nicer error messages,
or developer tools such as a test runner or a LSP.
I'm tracking the missing features in this [notion page](https://ascandone.notion.site/7534c8c846414e0c9ad2906540af5bc4?v=1685e0defdde4b768a2fe071e4ffd056).
