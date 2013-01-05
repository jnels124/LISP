;; Chapter 13 

;; If you want to signal an error directly you can do so with the error command
;;   Do this only if the program encounters an error and can not continue
;;   This will interupt your lisp program unless you intercept the error 
;;     elsewhere
(error "foo")
#| ->
*** - foo
The following restarts are available:
ABORT          :R1      Abort debug loop
ABORT          :R2      Abort main loop
|#

;;The above is not a good way to signal conditions 
;; A better way is to first define a custom condion using define-condition
(define-condition foo () ()
  (:report (lambda (condition stream)
	     (princ "Stop FOOing arround, dumby!" stream))))
#|
Created a condition named foo 
When this condition is signaled, we supply a custom function, the lambda 
  expression, that will be called to handle this condition
|#

;; Intercepting conditions

;; When you create a condition with define-condition, it is given a name(like foo).
;;  This name can be used by higher-level parts of the program to intercept and handle
;;     the condition
(defun bad-function ()
  (error 'foo))
(handler-case (bad-function)
	      (foo () "somebody signaled foo!")
              (bar () "somebody signaled bar!"))
#|
The first thing that goes inside a handler-case is the piece of code that may 
  signal the conditions that need to be handled 
In this case we want to watch the code in bad-function
|#

;; Protecting resources

;; Use unwind-protect to tell the lisp compiler to run this code no matter what happens
;;  You can usually avoid using this command by relying on the with- macros which already
;;    call unwind.
(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' no matter what"))