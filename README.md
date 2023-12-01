fn
==

A couple of lambda shorthand macros. Their goal is to be used in cases where the word 'lambda and args are longer than the body of the lambda. It fixes this by adding implicit arguments.

    (fn* (+ _ _))  -->  (lambda (_) (+ _ _))

    (fn* (+ _ _1))  -->  (lambda (_ _1) (+ _ _1))

    (fn* (subseq _@ 0 2))  -->  (lambda (&rest _@) (subseq _@ 0 2))


The λ reader macro gives you the clojure like syntax.

    λ(+ _ _)  -->  (lambda (_) (+ _ _))

    λ(+ _ _1)  -->  (lambda (_ _1) (+ _ _1))

    λ(subseq _@ 0 2)  -->  (lambda (&rest _@) (subseq _@ 0 2))

I REALLY dont like adding reader macros, but as λ is such a rarely used character I dont feel too bad about it.

To enable this reader macro, evaluate the following code:

    (named-readtables:in-readtable :fn.reader)

Finally `fn~` and `fn~r` are functions for partial application.
fn+ is for composing functions

Emacs
-----
If you are using emacs and want Meta-l to write the λ symbol, add the following to your .emacs file

    (global-set-key (kbd "M-l") (lambda () (interactive) (insert (make-char 'greek-iso8859-7 107))))

Some curiosities
----------------

`λ_` the identity function

	(mapcar λ_ '(1 2 3)) -> (1 2 3)

`λ``(1 ,_ 3)` list building functions

	(mapcar λ`(1 ,_ 3) '(1 2 3)) -> ((1 1 3) (1 2 3) (1 3 3))

`λ1` a function that takes no args an returns 1

    (funcall λpi) -> 3.141592653589793d0
