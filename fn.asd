;;;; fn.asd

(asdf:defsystem #:fn
  :description "Some macros for lambda brevity"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "Public Domain"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:trivial-macroexpand-all
               #:named-readtables)
  :components ((:file "package")
               (:file "fn")))
