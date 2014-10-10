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
  (cond ((atom form) (when (eql #\% (aref (format nil "~a" form) 0))
                       form))
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

(defmacro λ (&body body) 
  (if (consp (first body))
      `(lambda ,(first body) ,@(rest body))
      `(fn% ,body)))


;; Partial Application
;; -------------------
;; {TODO} Need to profule multiple-value-call to see if it is any better 
;;        than append

(defun λ_ (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (λ (&rest rest-of-the-args)
     (multiple-value-call function
       (values-list args) 
       (values-list rest-of-the-args))))

(define-compiler-macro λ_ (function &rest args)
  `(let ((function ,function))
     (declare (optimize (speed 3) (safety 1) (debug 1))
              (function function))
     (λ (&rest rest-of-the-args)
        (declare (optimize (speed 3) (safety 1) (debug 1)))
        (apply ,@args rest-of-the-args))))

(defun fn_ (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (λ (&rest rest-of-the-args)
     (multiple-value-call function
       (values-list args) 
       (values-list rest-of-the-args))))

(define-compiler-macro fn_ (function &rest args)
  `(let ((function ,function))
     (declare (optimize (speed 3) (safety 1) (debug 1))
              (function function))
     (λ (&rest rest-of-the-args)
        (declare (optimize (speed 3) (safety 1) (debug 1)))
        (apply ,@args rest-of-the-args))))

(defun λ_r (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (λ (&rest rest-of-the-args)
     (multiple-value-call function
       (values-list rest-of-the-args)
       (values-list args) )))

(defun fn_r (function &rest args)
  "Partially apply args to function"
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (function function))
  (λ (&rest rest-of-the-args)
     (multiple-value-call function
       (values-list rest-of-the-args)
       (values-list args) )))
