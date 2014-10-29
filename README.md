fn_
===

A couple of lambda shorthand macros. Their goal is to be used in cases where the word 'lambda and args are longer than the body of the lambda. It fixes this by adding implicit arguments.

    (fn% (+ % %))  -->  (lambda (%) (+ % %))

    (fn% (+ % %1))  -->  (lambda (% %1) (+ % %1))

    (fn% (subseq %@ 0 2))  -->  (lambda (&rest %@) (subseq %@ 0 2))


For a while we have the λ macro. This is meant to be equivilent to clojures `#(+ % %1)` shorthand syntax for lambdas but without being a reader-macro.

    (λ + % %)  -->  (lambda (%) (+ % %)) 

    (λ + % %1)  -->  (lambda (% %1) (+ % %1))

    (λ subseq %@ 0 2)  -->  (lambda (&rest %@) (subseq %@ 0 2)) 

However in the above case it means that the function name is the second symbol in the form which is a bit gross and also breaks autocompletion. 
To deal with this I have added one reader-macro.

The λ reader macro is gives you the clojure like syntax.

    λ(+ % %)  -->  (lambda (%) (+ % %)) 

    λ(+ % %1)  -->  (lambda (% %1) (+ % %1))

    λ(subseq %@ 0 2)  -->  (lambda (&rest %@) (subseq %@ 0 2)) 

I REALLY dont like adding reader macros, but as λ is such a rarely use character I dont feel too bad about it. 


Finally `fn_`, `fn_r`, `λ_` and `λ_r` are functions for partial application
        
Emacs
-----
If you are using emacs and want Meta-l to write the λ symbol, add the following to your .emacs file

    (global-set-key (kbd "M-l") (lambda () (interactive) (insert (make-char 'greek-iso8859-7 107))))
