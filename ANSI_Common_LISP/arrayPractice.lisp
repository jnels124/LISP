;; Declares an array of size 3
(make-array 3)

;; Here is how you would obtain item at index 1
(defparameter x (make-array 3)) ;; nothing but nil values right now

;; To set values in array use setf and aref
(defparameter x (make-array 3))
(setf (aref x 1) 'foo)

;;get Item just added 
(aref x 1)





