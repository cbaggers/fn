;;;; fn_.asd

(asdf:defsystem #:fn_
  :serial t
  :description "Some macros for lambda brevity"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "Public Domain"
  :depends-on (#:macroexpand-dammit)
  :components ((:file "package")
               (:file "fn_")))

