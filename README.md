fn_
===

A couple of lambda shorthand macros

If you are using emacs and want Meta-l to write the λ symbol, add the following to your .emacs file

    (global-set-key (kbd "M-l") (lambda () (interactive) (insert (make-char 'greek-iso8859-7 107))))