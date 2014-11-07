fn_
===

A couple of lambda shorthand macros. Their goal is to be used in cases where the word 'lambda and args are longer than the body of the lambda. It fixes this by adding implicit arguments.

    (fn% (+ % %))  -->  (lambda (%) (+ % %))

    (fn% (+ % %1))  -->  (lambda (% %1) (+ % %1))

    (fn% (subseq %@ 0 2))  -->  (lambda (&rest %@) (subseq %@ 0 2))


The λ reader macro is gives you the clojure like syntax.

    λ(+ % %)  -->  (lambda (%) (+ % %)) 

    λ(+ % %1)  -->  (lambda (% %1) (+ % %1))

    λ(subseq %@ 0 2)  -->  (lambda (&rest %@) (subseq %@ 0 2)) 

I REALLY dont like adding reader macros, but as λ is such a rarely use character I dont feel too bad about it. 


Finally `fn_` and `fn_r` are functions for partial application.
fn+ is for composing functions
        
Emacs
-----
If you are using emacs and want Meta-l to write the λ symbol, add the following to your .emacs file

    (global-set-key (kbd "M-l") (lambda () (interactive) (insert (make-char 'greek-iso8859-7 107))))
