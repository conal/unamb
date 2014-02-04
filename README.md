This package contains the "unambiguous choice" operator `unamb`, which wraps thread racing up in a purely functional, semantically simple wrapper.
Given any two arguments `u` and `v` that agree unless bottom, the value of `unamb u v` is the more terminating of `u` and `v`.
Operationally, the value of `unamb u v` becomes available when the earlier of `u` and `v` does.
The agreement precondition ensures unamb's referential transparency.

`unamb` and a sample use appeared in the paper [*Push-pull functional reactive programming*](http://conal.net/papers/push-pull-frp/).
I moved it to its own
package in order to encourage experimentation.

Note that `unamb` correctly implements the least-upper-bound operation (on information content), also known as "`lub`" or "`(⊔)`", but only for *flat* types (in which values are completely undefined or completely defined).
I suggest using the [lub](http://hackage.haskell.org/package/lub) package instead of unamb.
(Also [on GitHub](http://github.com/conal/lub).)
