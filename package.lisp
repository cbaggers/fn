;;;; package.lisp

(defpackage #:fn
  (:use #:cl #:trivial-macroexpand-all)
  (:export :fn% :fn~ :fn~r :fn+ :fn-reader))
