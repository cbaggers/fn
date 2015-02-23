;;;; fn_.lisp

(in-package #:fn_)

(defun flatten (tree)
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun walk-for-% (form)
  (cond ((atom form) (let ((strnd (format nil "~a" form)))
                       (when (and (symbolp form)
                                  (eql #\% (aref strnd 0)))
                         form)))
        ((eql (first form) 'quote) nil)
        (t (remove nil (mapcar #'walk-for-% form)))))

(defun get-%-symbs (form)
  (flatten (walk-for-% (macroexpand-dammit:macroexpand-dammit form))))

(defun %-arg-num (arg)
  (let ((name (symbol-name arg)))
    (if (= (length name) 1)
        0
        (parse-integer (subseq name 1)))))

(defun sequential (nums &optional (base 0))
  (cond ((null nums) t)
        ((and base (not (find base nums))) nil)
        (t (not (null (reduce (lambda (x y) (and x y (if (= (- y x) 1) y nil)))
                              nums))))))

(defmacro fn% (&body body)
  (let* ((raw-args (get-%-symbs body))
         (at-form (find "%@" raw-args :key #'symbol-name :test #'equal))
         (raw-args (remove "%@" raw-args :key #'symbol-name :test #'equal))
         (args (sort (remove-duplicates raw-args)
                     (lambda (x y) (< (%-arg-num x) (%-arg-num y))))))
    (if (sequential (sort (remove-duplicates (mapcar #'%-arg-num args)) #'<))
        (if at-form
            (if args
                `(lambda (&rest ,at-form)
                   (destructuring-bind ,args ,at-form ,@body))
                `(lambda (&rest ,at-form) ,@body))
            `(lambda ,args ,@body))
        (error "non sequential args"))))

;; Partial Application
;; -------------------
;; {TODO} Need to profule multiple-value-call to see if it is any better
;;        than append

(defun fn_ (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (lambda (&rest rest-of-the-args)
     (multiple-value-call function
       (values-list args)
       (values-list rest-of-the-args))))

(define-compiler-macro fn_ (function &rest args)
  (let ((gfunc (gensym "function")))
    `(let ((,gfunc ,function))
       (declare (optimize (speed 3) (safety 1) (debug 1))
                (function ,gfunc))
       (lambda (&rest rest-of-the-args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (apply ,gfunc ,@args rest-of-the-args)))))

(defun fn_r (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (lambda (&rest rest-of-the-args)
     (multiple-value-call function
       (values-list rest-of-the-args)
       (values-list args) )))

(defun fn+ (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (unless (and (functionp function)
               (every #'functionp more-functions))
    (error "fn+ can only compose functions"))
  (reduce (lambda (f g)
            (lambda (&rest arguments)
              (declare (optimize (speed 3) (safety 1) (debug 1))
                       (dynamic-extent arguments)
                       (function f g))
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

(defun range (end &optional (start 0))
  (loop :for i :from start :upto end :collect i))


(defun lambda-reader (stream char)
  (declare (ignore char))
  (let* ((body (read stream t nil t))
         (body (if (and (= 1 (length body))
                        (or (and (symbolp (first body))
                               (or (equal (symbol-name (first body)) "%")
                                   (equal (symbol-name (first body)) "%@")))
                            (constantp (first body))))
                   (first body)
                   body)))
    (list 'fn% body)))

(named-readtables:defreadtable fn_lambda
  (:merge :standard)
  (:macro-char #\GREEK_SMALL_LETTER_LAMDA #'lambda-reader t))
