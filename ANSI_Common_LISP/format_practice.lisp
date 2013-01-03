;; Formatting -> Chapter 11

;; Format can also loop through data using ~{ and ~}
;; General format -> 
;; (format destination-param "control string ~? more text" value-param)
;;                                       The ~ represents start of control sequence
;;                                         : the ? must be a valid control sequence
;; Vaild destination params : 
;;         nil-> Just creates a string 
;;         t-> Print to the console 
;;         stream-> output to the stream
;;

;; Vaild control sequences :  
;;         ~$ -> monetary floating point number 
;;         ~s -> Same behaviour as prin1 
;;         ~a -> Same behavior as princ
;;         ~% -> Same as using (terpri) which creates a new line in all cases
;;         ~& -> Same as using (fresh-line) which creates a new line only if needed
;;              both ~% and ~& can be prefixed with a number to indicate the number
;;                 of new lines
;;         ~t -> Display items in a table
;;                 takes a parameter that specifies the column position at which formatted 
;;                      value should appear

;; The following are used for controlling the appearence of numbers :
;;         ~x -> Displays a number in hexadecimal form
;;         ~b -> Displays a number in binary form
;;         ~d -> Display a number as decimal
;;         ~f -> Formats floating point numbers 
;;               1st param to ~f -> formats restult to the exact width
;;               2nd param to ~f -> controls the number digits diplayed after the decimal
;;               3rd param to ~f -> Causes the number to be scaled by a factor of 10
;;         
;; Control sequences can accept multiple paramaters
;;          1st param -> Controls the final width of final formated string 
;;          2nd param -> Tells format command to add spaces in groups of specified integer
;;          3rd param -> Controls the exact number of spaces to add to the end of formatted string
;;          4th param-> Specifies which character will be used for padding 
;; EX:   
(format t "Add onion rings for only ~$ dollars more!" 1.5) 
  ;; -> Add onion rings for only 1.50 dollars more!


(princ (reverse
	(format nil "Add onion rings for only ~$ dollars more!" 1.5)))
  ;;-> !erom srallod 05.1 ylno rof sgnir noino Add
  ;;  The format function returns a string to the reverese funtion which is then printed
  ;;     to the screen by using princ. Rememaber princ prints things to the screen so a 
  ;;     human can easily understand it 


;; Using ~s control sequence
(format t "I am printinting ~s in the middle of this sentence" "foo")
;;     -> I am printing "foo" in the middle of this sentence

;; Using ~a control sequence 
(format t "I am printing ~a in the middle of this sentence" "foo")
;;     -> I am printing foo in the middle of this sentence.

;; Manipulating contol sequence with digit. Digit represents length to pad result to
(format t "I am printing ~10a within 10 spaces of room." "foo")
;;     -> I am printing foo       within 10 spaces of room.
;;     This control sequence pads the value paramter's result to the right 

;; To pad the result to the left do this 
(format t "I am printing ~10@a within 10 spaces of room." "foo")
;;     -> I am printing       foo within 10 spaces of room.


;;This second paramater tells the format command to add spaces in groups
;;      of three until the goal width of 10 is reached. In this example 9 spaces
;;        were added to the formatted value for a total width of 12

;; Keep in mind the second param to the control sequence is rarely used 
(format t "I am printing ~10,3a within ten (or more) soaces of room." "foo")
;;     -> I am printing foo          within ten (or more) spaces of room


;; Regardless of length, formats the final value by appending exactly the 
;;  Speciifed number of spaces to the end of the value.
(format t "I am printing ~,,4a in the middle of this sentence" "foo")
;;     -> I am printing foo     in the middle of this sentence
;;         -> in this case 4 spaces are added to the end of foo

;;Padds the result with ! instead of spaces. 
(format t "The word ~,,4,'!a feels very important." "foo")
;;     -> The word foo!!!! feels very important.

;; Same as above but result is padded to the left instead
(format t "The word ~,,4,'!@a feels very important." "foo")
;;     -> The word !!!!foo feels very important.

;; Prints number in hexadecimal form 
(format t "The number 1000 in hexadecimal is ~x" 1000)
;;      -> The number 1000 in hexadecimal is 3E8

;; Prints number in binary form 
(format t "The number 1000 in hexadecimal is ~b" 1000)
;;      -> The number 1000 in binary is 1111101000

;; Prints number in decimal form
;; although the number is alredy in decimal form, specifying it this way allows
;;     additonal formating options to be used 
(format t "The number 1000 in decimal is ~d" 1000)
;;      ->  The number 1000 in decimal is 1000
;;In this example, ~a would have given the same thing but see the following

;; Prints out decimal with commas between digit groups
;;    The : triggers this feature
(format t "Numbers with commas in them are ~:d" 1000000)
;;      -> Numbers with commas in them are 1,000,000

;; You can still set the padding paramater like before.
(format t "I am printing ~10d within ten spaces of room." 1000000)
;;      -> I am printing     1000000 within ten spaces of room.

;; To change the character used for padding, pass the desired character as the 
;;       second parameter.
(format t "I am printing ~10,'xd within ten spaces of room" 1000000)
;;      -> I am printing xxx1000000 within ten spaces of room 

;; Automatically rounds result to specified length
(format t "PI can be estimated as ~4f" 3.141593)
;;      -> PI can be estimated as 3.14

;; Will print the specifed number of values after the decimal point
(format t "PI can be estimated as ~,4f" 3.141593)
;;      -> PI can be estimated as 3.1416

;; Since pi is a common lisp constant you can also rewrite above command as 
(format t "PI can be estimated as ~,4f" pi)
;;      -> PI can be estimated as 3.146

;; Scales the number by factors of 10
(format t "Percentages are ~,,2f percent better than fractions" 0.77)
;;      -> Percentages are 77.0 percent better tha fractions

;; Use ~$ to format currencies 
(format t "I wish I had ~$ dollars in my bank account." 1000000.2)

;; THE FOLLOWING EXAMPLES DEAL WITH PRINTING MULTIPLE LINES OF OUTPUT
;;   There are two different commands for starting a new line durring printing
;;      terpri(~%) -> tells lisp to terminate the current line and start a new one for 
;;                for pinting the rest of the output.
;;      fresh-line(~&) -> Starts a new line only if the cursor in the repel isn't already
;;                    at the front of a new line 


;; Example of terpri
(progn (princ 22)
       (terpri)
       (princ 33))
;;       -> 22
;;          33

(progn (princ 22)
       (terpri)
       (terpri)
       (princ 33))
;;       -> 22
;;
;;          33

;; Example of fresh-line
(progn (princ 22)
       (fresh-line)
       (princ 33))
;;       -> 22
;;          33

(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (princ 33))
;;       -> 22
;;          33

(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))
;;       -> this is on one line
;;
;;          this is on another line

(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))
;;       -> this is on one line
;;          this is on another line

;; Prints a specified number of fresh lines
(format t "this will print ~5%on two lines spread far apart")
;;       -> this will print 
;;
;;
;;
;;
;;
;;          on two lines spread far apart 

;; Function returns a random animal. Function used in
;;    the following text justification examples.
(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "Kangaroo")))

;; The control sequenece creats a table with three columns
;;    the first columns value will be at character position 5
;;    the second column's value will start at 15 
;;    the third column's value will start at 25
;; You can create as many columns as you want by continuing to specify additonal control sequences
(loop repeat 10 
      do (format t "~5t~a ~15t~a ~25t~a~%"
		 (random-animal)
		 (random-animal)
	         (random-animal)))


;; This will space the animals equally apart on each line
;;    this may format each line but the columns are still a mess 
(loop  repeat 10
       do (format t "~30<~a~;~a~;~a~>~%"
		  (Randomc-animal)
		  (random-animal)
		  (random-animal)))    
;; ~30<   -> Start justifying.   Whole line will be 30 characters
;; ~a     -> put one of the three values here
;; ~;     -> Start new item to justify.... indicates where to start spacing
;; ~a     -> put one of the three values here
;; ~;     -> Start new item to justify
;; ~a     -> put one of the three values here
;; ~>     -> Stop justifying 
;; ~%     -> Print a new line character

;; we can create alligned columns by adding the :@ flag to the jutification
;;    this example only formats a single column
(loop repeat 10
      do (format t "~30:@<~a~>~%" (random-animal)))

;; Centers on each line with extra space at left and right ends
;;    This gets you a little close to having fully formatted rows and columns
(loop repeat 10
      do (format t "~30:@<~a~;~a~;~a~>~%" 
		 (random-animal)
		 (random-animal)
		 (random-animal)))
;;    -> lines are still wavy because we're alligning the values within a single line
;;        without telling format to arrange the values using three centered columns

;; Creates three sepearate sections of 10 characters each and centers one value
;;   in each of the 10 character sections creating a perfectly formatted table 
(loop repeat 10
      do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%" 
		 (random-animal)
		 (random-animal)
		 (random-animal)))


;; List of animals to use in looping with format command example
(defparameter *animals* 
  (loop repeat 10 
	collect (random-animal)))

;; now use format to loop through animals
(format t "~{I see a ~a! ~}" *animals*)

;; You can also grab more than one item from a list 
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)

;; Uses some old and some new control sequences together to show 
;;   how they can be combined for complex behavior
(format t "|~{~<|~%|~,33:;~2d ~>~}|"
	(loop for x
	      below 100
	      collect x))
;;| 0  1  2  3  4  5  6  7  8  9 |
;;|10 11 12 13 14 15 16 17 18 19 |
;;|20 21 22 23 24 25 26 27 28 29 |
;;|30 31 32 33 34 35 36 37 38 39 |
;;|40 41 42 43 44 45 46 47 48 49 |
;;|50 51 52 53 54 55 56 57 58 59 |
;;|60 61 62 63 64 65 66 67 68 69 |
;;|70 71 72 73 74 75 76 77 78 79 |
;;|80 81 82 83 84 85 86 87 88 89 |
;;|90 91 92 93 94 95 96 97 98 99 |

;;   -> In this case the ~{ and ~} are not used to justify text but instead uses
;;       them to divide the resulting text into pieces
;;   -> Using the ~:; inside the justification seqence caues the text to be broken down into 
;;       equal lengths
;;    -> see page 232 in land of lisp for more info 
