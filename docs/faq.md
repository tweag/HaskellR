---
id: faq
---

# The HaskellR FAQ

#### Why do I get a stack limit error when using GHCi and how can I fix it?

R incorporates a stack usage check. This check only works reliably
when run from a program's main thread. By default, GHCi evaluates all
code in a worker thread as a form of isolation, rather than on the
main thread. This leads to errors like the following when evaluating
code in GHCi:

```
Error: C stack usage 140730332267004 is too close to the limit
```

The fix is to turn off GHCi sandboxing, by passing the
`-fno-ghci-sandbox` when invoking GHCi. This forces GHCi to evaluate
the code from its main thread.

#### Why does loading the HaskellR code itself in GHCi fail in GHC
     7.10.2 or earlier?

For instance, something like the following to compile `inline-r` and
friends in GHCi fails:

```
$ stack repl
```

This is a known issue with GHC, tracked as
[Trac ticket #10458][trac-10458].

[trac-10458]: https://ghc.haskell.org/trac/ghc/ticket/10458
