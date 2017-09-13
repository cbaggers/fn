;;;; fn.asd

(asdf:defsystem #:fn
  :description "Some macros for lambda brevity"
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "Public Domain"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:named-readtables)
  :components ((:file "package")
               (:file "mad")
               (:file "fn")))
