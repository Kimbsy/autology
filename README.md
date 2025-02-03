# Autology

A hot take on Lisp metaprogramming, `Autology` is a Lisp with access
to its own interpreter.

# What is it?

Autology is a functional interpreted Lisp language, written in Clojure.

The Autology interpreter function is defined as a data structure which is available to the Autology program, it is bound to the variable `*i*`.

Whenever an expression needs to be evaluated, Autology will retrieve the current value of `*i*` from the current lexically scoped execution environment and use it to evaluate the expression.

By binding new values of `*i*` we can modify the behaviour of the Autology language while our program is running.

# Why have you done this?

Mainly for fun and to explore a neat idea.

Autology is not a particularly useful language as-is, its very slow and resource intensive.

Macros are the normal tool for Lisp languages to modify their syntax or behaviour, but these are normally only available at compile time. Autology allows dynamic rewriting of the language at runtime.

Even other non-Lisp languages are able to define some form of Domain Specific Language to allow programmers to express problems more clearly, however these are generally quite restrictive in scope. Autology allows full ad-hoc re-writing of the syntax of the language, as well as defining new features or removing existing ones.

# What can you do with it?



# Run tests

```bash
clojure -X:test
```
