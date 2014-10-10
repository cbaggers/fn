fn_
===

A couple of lambda shorthand macros. Their goal is to be used in cases where the word 'lambda and args are longer than the body of the lambda. It fixes this by adding implicit arguments.

    (fn% (+ % %))  -->  (LAMBDA (%) (+ % %))

    (fn% (+ % %1))  -->  (LAMBDA (% %1) (+ % %1))

Next we have the λ macro. This is meant to be equivilent to clojures #(+ % %1) shorthand syntax for lambdas. However I am REALLY adverse to adding reader macros unless I absolutely have to. I know in this case it means that the function name is the second symbol in the form which is a bit icky..but given λ is a small and obvious symbol I havent found it hurts the speed I mentally parse my code.

    (λ + % %)  -->  (LAMBDA (%) (+ % %)) 

    (λ + % %1)  -->  (LAMBDA (% %1) (+ % %1))



If you are using emacs and want Meta-l to write the λ symbol, add the following to your .emacs file

    (global-set-key (kbd "M-l") (lambda () (interactive) (insert (make-char 'greek-iso8859-7 107))))