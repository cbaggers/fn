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
             (alexandria:flatten form)))

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

