(loop for i
      below 5
    sum i)  ;;  -> 10

(loop for i
      from 5
      to 10
    sum i) ;; -> 45

(loop for i 
      in '(100 20 32) 
    sum i) ;; -> 152

(loop for i 
      below 5
    do (print i)) ;; -> 0, 1, 2, 3, 4 -> All on separate lines ->print

;; You can also loop under conditions using :

(loop for i
      below 10
    when (oddp i)
    sum i)

;;Cartesia product-> loop once for every possible 2 or more ranges
(loop for x 
      below 10                ;; 10 lists of
    collect (loop for y            
        	  below 10    ;; 10 items each 
		collect (+ x y)))
		  