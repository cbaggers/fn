;;;; fn_.asd

(asdf:defsystem #:fn_
  :serial t
  :description "2 macros for lambda brevity"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :components ((:file "package")
               (:file "fn_")))

