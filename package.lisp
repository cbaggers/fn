;;;; package.lisp

(defpackage #:fn
  (:use #:cl #:macroexpand-dammit)
  (:export :fn% :fn~ :fn~r :fn+ :fn-reader))

