;;;; fn.asd

(asdf:defsystem #:fn
  :serial t
  :description "Some macros for lambda brevity"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "Public Domain"
  :depends-on (#:macroexpand-dammit
               #:named-readtables)
  :components ((:file "package")
               (:file "fn")))

