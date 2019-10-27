# fibonacci
Common Lisp implementation of fibonacci numbers.

This started out as a trivial implementation of the computation of Fibonacci
numbers, but then I decided that I wanted to compute arbitrary numbers, which
the trivial implementation cannot achieve in a satisfactory time.

To try this out open SLIME:

```common-lisp
(load-system 'fibonacci)
(test-system 'fibonacci)
(fibonacci:fastfib 89)
```
