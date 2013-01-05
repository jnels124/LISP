;; A simple web server Chapter 

;; Decodes ASCII codes
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer  ;; Convert string to int
	       (coerce (list c1 c2) 'string)
	       :radix 16  ;;Tells parse-integer to parse a hexadecimal number 
	       :junk-allowed t))) ;; Tells function to return nil when an invalid number is passed in instead of signal error
    (if code
	(code-char code) ;; lisp function that converts hex number to actual character user entered
      default)))

;; Decode HTTP escape codes
(defun decode-param (s)
  (labels ((f (lst) ;; Recursively processes characters
	      (when lst
		(case (car lst) ; Checks if first character is a + or %.
		      (#\% (cons (http-char (cadr lst) (caddr lst)) ;; Know next value is a ASCII value represented in hex
				 (f (cdddr lst))))
;; Per HTTP rules if a value in a request param is a + it should be converted into a space
		      (#\+ (cons #\Space (f (cdr lst))))
		      (otherwise (cons (car lst) (f (cdr lst))))))));; any other char passes through unchanged
;; Allows recursion to work by forcing string into a list of characters and passing this list to f
	  (coerce (f (coerce s 'list)) 'string))) 

;; Decode list of request params given as name value pairs
(defun parse-params (s)
;; Finds first occurence of an & and = in the string using the position function
  (let ((i1 (position #\= s))
	(i2 (position #\& s)))
;; If a name/value pair is found (we know this is true if an = sign was found in the string and stored in l1
;;    Uses the intern function to convert the name into a lisp symbol
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
;; We cons this name to the value of the param which is decoded with above decode-param function
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2)))))) ;; make recursive call on remainder of the string 
	  ((equal s "") nil)
	  (t s))))
;; -> Returns an alitst of name value pairs.
;;   Storeing the values in alist makes it easy to access a specific variable when needed

;; Process first line of request header
(defun parse-url (s)
  (let* ((url (subseq s 
		      (+ 2 (position #\Space s))
;; Uses the strings delimiting spaces to find and extract the URL 
		      (position #\Space s :from-end t)))
;; Checks URL for a ? which indicate there may be request params that need to be handled
	 (x (position #\? url)))
    (if x
;; If params exist, they need to be extracted and parsed using the parse-params function
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
;; Otherwise the URL is just returned
        (cons url '())))) 

;; -> This function skips of the request method (i.e POST, GET)... A more complete server would extract this info as well

;; Convert the remaining lines of the request header into an alist
(defun get-header (stream) 
;; reads first line of stream 
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i 
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2))))))) ;; and converts it to a key value pair based on the location of the colon
    (when h
;; recurses to convert additonal lines in the header.
;; If a line is encoutered that doesn't conform to a header line it means we have reached a blank line 
;;     and the function terminates.
      (cons h (get-header stream))))) 

;; Post requests will often have parameters stored beneath the header in an area known as the "request body"
;;    or "request content" 
;; This funtion extracts these additonal parameters
(defun get-content-params (stream header)
;; Searches header for a value called content-length, which tells you the length of the string that contains these content params
  (let ((length (cdr (assoc 'content-length header)))) 
;; If length exists we know there are parameters to parse
    (when length
      ;; Create a string of the specified length 
      (let ((content (make-string (parse-integer length))))
	;; Use read sequence to fill above string with characters from the stream 
	(read-sequence content stream)
	;; Run result through parse-params function to translate params into a cleaned up alist
	(parse-params content)))))

;; Heart of server
;; request-handler is supplied by the creator of the website that wants to use this web server
;;    when a request is received over the network, it parses the request into clean Lisp data structures
;;     and this passes this request info to request-handler
;;    The request handler then displays the correct HTML
(defun serve (request-handler)
 ;; Create a socket bound to port 8080 .. This is a commonly used port for serving web pages
;;      especially when they are under devlopment .
;        - port 80 is usually used for a website/webserver 
  (let ((socket (socket-server 8080)))
;; Call unwind to ensure that no matter what happens the server runs  and socket-server-close will be called to free the socket
    (unwind-protect ;; use with-open-stream to ensre the stream will be properly closed
	;; open a stream for a client that wants to access the server
	(loop (with-open-stream (stream (socket-accept socket))
				;; Read and parse the website request that clone has made using above reading 
				;;   and parseing functions
				(let* ((url    (parse-url (read-line stream)))
				       (path   (car url))
				       (header (get-header stream))
				       (params (append (cdr url)
						       (get-content-params stream header)))
				       (*standard-output* stream)) ;; able to use standard output and redirect it to a stream
				  (funcall request-handler path header params))))
      (socket-server-close socket))))

;; Creates a simple website to test the server
;;   This function only supports a singe webpage
(defun hello-request-handler (path header params)
  (if (equal path "greeting") ;; Verify that "greeting" is the page requested.If not an apolygy is printed (last line)
      (let ((name (assoc 'name params))) ;; otherwise, check the request params to see if we know the user's name
	(if (not name)
	    (princ "<html><form> What is your name?<input name='name' />
</form></html>");; If not, we ask the user to enter it in a form
	  (format t "Nice to meet you, ~a!</html>" (cdr name))));; If we do know the users name, they are greeted
    (princ "Sorry... I don't know that page.")))