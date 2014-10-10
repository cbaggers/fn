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

(defun get-%-symbs (form)
  (remove-if (lambda (x) (not (eql #\% (aref (format nil "~a" x) 0))))
             (flatten form)))

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
  (let ((args (sort (remove-duplicates (get-%-symbs body)) 
                    (lambda (x y) (< (%-arg-num x) (%-arg-num y))))))
    (if (sequential (sort (remove-duplicates (mapcar #'%-arg-num args)) #'<))
        `(lambda ,args ,@body)
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
