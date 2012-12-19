;;Chapter 5 Land of Lisp 


;;Different rooms in the game
(defparameter *wizard-nodes* '((living-room (you are in the living-room.
					     a wizard is snoring loudly on the couch.))
			       (garden (you are in a beautiful garden.
					there is a well in front of you.))
			       (attic (you are in the attic.
				       there is a giant welding torch in the corner.))))

;;Room access points
(defparameter *wizard-edges* '((living-room (garden west door)
				            (attic upstairs ladder))
			       (garden (living-room east door))
			       (attic (living-room downstairs ladder))))

;;Objects that can be picked up off the floor
(defparameter *objects* '(whiskey bucket frog chain))

;;Location of objects
(defparameter *object-locations* '((whiskey living-room)
     				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;;Keep track of location
(defparameter *location* 'living-room)

;;Create look up key for nodes
(defun describe-location(location nodes)
   (cadr(assoc location nodes)))

;;Describes the path to a room,, describes only a single room
(defun describe-path(edge)
   `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;Describes all paths to a given room from the current location
(defun describe-paths(location edges)
   (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;Lists objects that are visible from a given location
(defun objects-at (loc objs obj-locs)
   (labels ((at-loc-p (obj)
	      (eq (cadr (assoc obj obj-locs)) loc)))
     (remove-if-not #'at-loc-p objs)))

;;Describe object
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
		`(you see a ,obj on the floor.)))
     (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

 ;;Looks arround the room 
(defun look () 
   (append (describe-location *location* *wizard-nodes*)
	   (describe-paths *location* *wizard-edges*)
	   (describe-objects *location* *objects* *object-locations*)))

;;Walk arround the room 
(defun walk (direction) 
   (let ((next (find direction
		     (cdr (assoc *location* *wizard-edges*))
		     :key #'cadr)))
   (if next 
       (progn (setf *location* (car next))
	      (look))
       '(you cannot go that way.))))

;; Pick up objects
(defun pickup (object)
   (cond ((member object 
		  (objects-at *location* *objects* *object-locations*))
	  (push (list object 'body) *object-locations*)
	    `(you are now carrying the ,object))
          (t '(you cannot get that.))))

;; Inventory of objects beign carried
(defun inventory() 
   (cons 'items- (objects-at 'body *objects* *object-locations*)))


;;Custom repel 
(defun game-repl () 
   (let ((cmd (game-read)))
       (unless (eq (car cmd) 'quit)
	   (game-print (game-eval cmd))
	   (game-repl))))

(defun game-read ()
   (let ((cmd (read-from-string
		    (concatenate 'string "(" (read-line) ")"))))
	(flet ((quote-it (x)
		       (list 'quote x)))
	    (cons (car cmd)(mapcar #'quote-it (cdr cmd))))))

;;Commands allowed by the game
(defparameter *allowed-commands* '(look walk pickup inventory))

;; Eval for game 
(defun game-eval (sexp)
     (if (member (car sexp) *allowed-commands*)
	 (eval sexp)
	 '(i do not know that command.)))


(defun tweak-text (lst caps lit)
 (when lst
(let ((item (car lst))
      (rest (cdr lst)))
  (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	((eql item #\") (tweak-text rest caps (not lit)))
	(lit (cons item (tweak-text rest nil lit)))
	(caps (cons (char-upcase item) (tweak-text rest nil lit)))
	(t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "()"
						   (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
(fresh-line))


