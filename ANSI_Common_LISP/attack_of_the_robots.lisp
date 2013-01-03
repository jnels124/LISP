(defun robots ()
  (loop named main  ;; by naming main , we can use "return from" to exit early 
	with directions = '((q . -65) (w . -64) (e . -63) (a . -1);; these are the 8 offsets when board is 64 wide
			    (d .   1) (z .  63) (x .  64) (c . 65))
	for pos = 544
	then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave:")
		    (force-output)
		    (let* ((c (read))
			   (d (assoc c directions)))
		      (cond (d (+ pos (cdr d)))
			    ((eq 't c) (random 1024)) ;;game board is 64 x 16 = 1024 ,,, choose random spot
			    ((eq 'l c) (return-from main 'bye)) ;; player wants to leave 
			    (t pos))))
	for monsters = (loop repeat 10 
			     collect (random 1024))
	then (loop for mpos in monsters 
		   collect (if (> (count mpos monsters) 1)
			       mpos ;; sort locations based on distance to player, then chomp off closest
			     (cdar (sort (loop for (k . d) in directions
					       for new-mpos = (+ mpos d)
					       ;; next four lines calculate the "manhattan distance" to the player
					       collect (cons (+ (abs (- (mod new-mpos 64)
								        (mod pos 64)))
								(abs (- (ash new-mpos -6)
									(ash pos -6))))
							     new-mpos))
					 '<
					 :key #'car))))
        when (loop for mpos in monsters ;; checks if all monsters are scrap
		   always (> (count mpos monsters) 1)) ;; more than one robot in the same place means they are scrap
	return 'player-wins
	do (format t
		   "~%|~{~<|~%|~,65:;~A~>~}|" ;; need to research this control sequences operation
		   (loop for p
			 below 1024  ;; loop through board positions
			 collect (cond ((member p monsters)
					(cond ((= p pos) (return-from main 'player-loses))
					      ((> (count p monsters) 1) #\#) ;; draw scrap for doubled-up robots
					      (t #\A)))
				       ((= p pos)
					#\@)
				       (t 
					#\ ))))))