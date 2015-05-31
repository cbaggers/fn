fn
==

A couple of lambda shorthand macros. Their goal is to be used in cases where the word 'lambda and args are longer than the body of the lambda. It fixes this by adding implicit arguments.

    (fn* (+ _ _))  -->  (lambda (_) (+ _ _))

    (fn* (+ _ _1))  -->  (lambda (_ _1) (+ _ _1))

    (fn* (subseq _@ 0 2))  -->  (lambda (&rest _@) (subseq _@ 0 2))


The λ reader macro is gives you the clojure like syntax.

    λ(+ _ _)  -->  (lambda (_) (+ _ _)) 

    λ(+ _ _1)  -->  (lambda (_ _1) (+ _ _1))

    λ(subseq _@ 0 2)  -->  (lambda (&rest _@) (subseq _@ 0 2)) 

I REALLY dont like adding reader macros, but as λ is such a rarely use character I dont feel too bad about it. 


Finally `fn~` and `fn~r` are functions for partial application.
fn+ is for composing functions
        
Emacs
-----
If you are using emacs and want Meta-l to write the λ symbol, add the following to your .emacs file

    (global-set-key (kbd "M-l") (lambda () (interactive) (insert (make-char 'greek-iso8859-7 107))))
