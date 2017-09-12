;;;; fn.lisp

(in-package #:fn)

(defvar *stop-on-symbols* '(quote lambda))

(defmacro fn* (form &environment env)
  (fn*-internals form env))

(defun fn*-internals (form env)
  (labels ((flatten (tree)
             (let (list)
               (labels ((traverse (subtree)
                          (when subtree
                            (if (consp subtree)
                                (progn
                                  (traverse (car subtree))
                                  (traverse (cdr subtree)))
                                (push subtree list)))))
                 (traverse tree))
               (nreverse list))))
    (let ((args
           (sort
            (remove-duplicates
             (remove
              nil
              (flatten
               (filter-tree
                (macroexpand-all form env)
                (lambda (x)
                  (and (symbolp x)
                       (char= #\_ (aref (symbol-name x) 0))))
                *stop-on-symbols*))))
            #'string<))
          (g (gensym)))
      (let ((at-arg (find "_@" args :test #'equal :key #'symbol-name))
            (args (remove "_@" args :test #'equal :key #'symbol-name)))
        (when args
          (unless (every #'valid-_-name args)
            (error "invalid underscore-arg name in ~s. Names must start with _ and be followed only by numbers indicating argument position." args))
          (unless (= (normalized-arg-count args) (length args))
            (error "The argument list ~a has holes. This is considered a malformed arg list" args)))
        (cond ((and at-arg args)
               `(lambda (&rest ,at-arg)
                  (destructuring-bind (,@args &rest ,g) ,at-arg
                    (declare (ignore ,g))
                    ,form)))
              (at-arg `(lambda (&rest ,at-arg) ,form))
              (t `(lambda ,args ,form)))))))

(defun normalized-arg-count (x)
  (let ((x (subseq (symbol-name (car (last x)))
                   1)))
    (if (equal x "")
        1
        (1+ (parse-integer x)))))

(defun valid-_-name (x)
  (let ((x (symbol-name x)))
    (and (char= (aref x 0) #\_)
         (every #'digit-char-p (subseq x 1)))))

(defun filter-tree (tree predicate &optional (stop-on nil))
  (if (atom tree)
      (when (and tree (funcall predicate tree))
        tree)
      (unless (and (symbolp (first tree))
                   (member (first tree) stop-on :test #'eq))
        (cons (filter-tree (first tree) predicate stop-on)
              (filter-tree (rest tree) predicate stop-on)))))

;; Partial Application
;; -------------------
;; {TODO} Need to profile multiple-value-call to see if it is any better
;;        than append

(defun fn~ (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (lambda (&rest rest-of-the-args)
    (multiple-value-call function
      (values-list args)
      (values-list rest-of-the-args))))

(define-compiler-macro fn~ (function &rest args)
  (let ((gfunc (gensym "function")))
    `(let ((,gfunc ,function))
       (declare (optimize (speed 3) (safety 1) (debug 1))
                (function ,gfunc))
       (lambda (&rest rest-of-the-args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (apply ,gfunc ,@args rest-of-the-args)))))

(defun fn~r (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (lambda (&rest rest-of-the-args)
    (multiple-value-call function
      (values-list rest-of-the-args)
      (values-list args))))

(defun fn+ (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (unless (and (functionp function)
               (every #'functionp more-functions))
    (error "fn+ can only compose functions"))
  (reduce (lambda (f g)
            (declare (type function f g))
            (lambda (&rest arguments)
              (declare (optimize (speed 3) (safety 1) (debug 1))
                       (dynamic-extent arguments))
              (funcall f (apply g arguments))))
          more-functions
          :initial-value function))

(define-compiler-macro fn+ (&rest functions)
  (labels ((sharp-quoted-p (x)
             (and (listp x)
                  (eql (first x) 'function)
                  (symbolp (second x)))))
    `(lambda (x) ,(reduce #'(lambda (fun arg)
                              (if (sharp-quoted-p fun)
                                  (list (second fun) arg)
                                  (list 'funcall fun arg)))
                          functions
                          :initial-value 'x
                          :from-end t))))

(defun lambda-reader (stream char)
  (declare (ignore char))
  (let* ((body (read stream t nil t)))
    (fn*-internals body nil)))

#-abcl
(named-readtables:defreadtable fn-reader
    (:merge :standard)
  (:macro-char #\GREEK_SMALL_LETTER_LAMDA #'lambda-reader t))

#-abcl
(named-readtables:defreadtable :fn.reader
    (:merge :standard)
	(:macro-char #\GREEK_SMALL_LETTER_LAMDA #'lambda-reader t))

#+abcl
(named-readtables:defreadtable fn-reader
    (:merge :standard)
  (:macro-char #\λ #'lambda-reader t))

#+abcl
(named-readtables:defreadtable :fn.reader
    (:merge :standard)
  (:macro-char #\λ #'lambda-reader t))
