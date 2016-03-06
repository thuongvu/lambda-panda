Lambda panda - lambda calculus programming language in Racket
====================================================
The goal of this racket module is to implement a lambda calculus interpreter in Racket.

Usage:
I recommend using [DrRacket], an IDE for writing code in Racket and running it.

Open `call-by-value-eval.rkt` in DrRacket.

Click `Run`.  When prompted, enter a lambda expression, such as `((λ(x)x)y))`, and it should return you a value `y`.


Resources:
----------
I owe lots to the works of:

- Matt Might who wrote a [great post] on implementing an interpreter based on lambda calculus.
- Chapter 5 of Ken Slonneger and Barry Kurtz's [Formal Syntax and Semantics of Programming Languages], a great introductory mathematical foundation to lambda calclus.
- Andrew Kuhnhausen and his series of [blog posts] about lamba calculus, which I first based my own implementation on.

[DrRacket]: https://racket-lang.org/
[great post]: http://matt.might.net/articles/implementing-a-programming-language/
[Formal Syntax and Semantics of Programming Languages]: http://homepage.cs.uiowa.edu/~slonnegr/plf/Book/Chapter5.pdf
[blog posts]: http://blog.errstr.com/2012/03/17/reduction/