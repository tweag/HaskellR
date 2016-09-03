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
the code from its main thread. The H wrapper around GHCi does this for
you.

If you get this error by using notebook, you can add `:set -fno-ghci-sandbox`
to `~/.ihaskell/rc.hs`. If the file didn't exist just create it.

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

#### How do I fix linker errors?

Most systems use the historic `ld.bfd` linker by default. However,
some versions of this linker has a [bug][ld-pie-bug] preventing the
linker from handling relocations properly. You might be seeing linker
errors like the following:

```
/usr/bin/ld: .stack-work/dist/x86_64-linux/Cabal-1.18.1.5/build/IHaskell/Display/InlineR.dyn_o: relocation R_X86_64_PC32 against symbol `ihaskellzminlinezmrzm0zi1zi0zi0_IHaskellziDisplayziInlineR_rprint5_closure' can not be used when making a shared object; recompile with -fPIC
    /usr/bin/ld: final link failed: Bad value
    collect2: error: ld returned 1 exit status
```

The fix is to either upgrade your linker, or better yet, switch to the
gold linker. On Ubuntu, you can do this with:

```
# update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
# update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
# update-alternatives --config ld
```

Stackage LTS Docker images use the `ld.gold` linker by default for
this reason, so aren't affected by the bug.

[ld-pie-bug]: https://sourceware.org/bugzilla/show_bug.cgi?id=17689
