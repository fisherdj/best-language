# The Best Programming Language

A repository showing how easy it is to make an extremely powerful programming language in very little time. lang1-orig, lang2-orig, and lang3-orig were all made in a span of 24 hours. lang1, lang2 and lang3 are cleaned-up equivalents.

## Building

Building the language requires Haskell. To compile with GHC, run:

```
ghc Language.hs
```

This should produce an executable `Language`. The `-f` flag loads a file and the `-e` flag evaluates an expression. To use a REPL for the language, type:

```
./Language -f test.scm -e (repl (current-env))
```

The REPL does not come provided with any exit mechanism. If one is really needed, try `(effect 'exit)` to crash the program.

## Features

The final language has Fexprs and virtual I/O with effects. Between these two it is easy to implement additional features including nondeterminism and state. The language is a Lisp dialect with a hand-written parser.

## Branches

* lang1-orig: the original language with lambda
* lang2-orig: the original language with fexprs
* lang3-orig: the original language with effects and virtual I/O
* lang1: a cleanup of lang1-orig
* lang2: a cleanup of lang2-orig
* lang3: a cleanup of lang3-orig
* lang3-cps: a variant of lang3 based on continuation-passing style as an implementation strategy
* master: lang3 plus a README

## Questions

* "Best" seems like a bold claim, can you back that up?

While the name is facetious there is some truth to the idea that this really is the best. This language has meta-programming features better than almost every language out there. This includes all languages with hygienic macros. Further, this language has an I/O model equivalent to the best in research (specifically: equivalent to algebraic effects with shallow effect handlers).

* Really, there's no language more powerful?

A fully-reflective variant would be strictly more powerful, but has theoretical issues that make it unsuitable for real programming.
