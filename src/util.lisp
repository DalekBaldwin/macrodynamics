(in-package :macrodynamics)

(defun get-assoc (item alist &rest keys &key key test test-not)
  "Like ASSOC but returns the cdr instead of the whole matching cons and
a second value indicating success or failure."
  (declare (ignore key test test-not))
  (let ((result (apply #'assoc item alist keys)))
    (cond
      ((null result)
       (values nil nil))
      (t
       (values (cdr result) t)))))

(defun update-alist (item value alist)
  "Non-destructively replace cdr of the cons whose car matches ITEM in ALIST
with VALUE, or insert a new cons if no car matches ITEM."
  (destructuring-bind (replacedp new-alist)
      (reduce (lambda (accum item)
                (destructuring-bind (replacedp new-alist) accum
                  (cond
                    (replacedp
                     (list replacedp (list* item new-alist)))
                    ((eql item (car item))
                     (list t (list* (cons item value) new-alist)))
                    (t
                     (list replacedp (list* item new-alist))))))
              alist
              :initial-value (list nil nil))
    (cond
      (replacedp
       (reverse new-alist))
      (t
       (list* (cons item value) alist)))))
