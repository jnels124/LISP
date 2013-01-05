;; Chapter 12 land of lisp 
;; Streams are a lisp data type that allow you to take some external 
;;    resource and make it look like just another piece of data you can 
;;    manipulate with your code.
;;     External resources can be multiple things such as a file on a disk
;;       , another computer on a network, or text in a console. 

;; Streams by type of resource
;;   Console Streams -> What you use when communicating with the repel 
;;   File streams -> Let us read and wite to files on hard drive
;;   Socket streams -> Communicate with other computers on a network 
;;   String streams -> Lets us send and recieve text from a lisp string
;;       String streams are unique because rather allow to communicate 
;;         with the outside world, instead they allow you to manipulate 
;;         strings in an interesting way

;; Streams by direction
;;   Output stream -> Writes data to a stream
;;      From most primitive level you can do 2 things :
;;        1. Check if the stream is valid 
;;        2. Push a new item onto the stream

;; Checks for a valid output stream. Stream associated with repel is *standard-output*
(output-stream-p *standard-output*)

;; Pushes a character onto the stream
(write-char #\x *standard-output*)

;;   Input stream-> Reads data 
;;     1. Check if stream is valid
;;     2. Pop an item off of the stream

;; Checks for a valid input stream. Stream associated with repel is *standard-input*
(input-stream-p *standard-input*)

;; Pop an item off of the stream 
(read-char *standard-input*) 

;; Using with-open-file is the best way to deal with files because it opens and closes files 
;; automatically so you dont have to worry about data leaks in these areas



;; Creates a file stream, "my-stream",  and writes "my data" to a file named streamData.txt
(with-open-file (my-stream "streamData.txt" :direction :output)
		(print "my data" my-stream))
;; my-stream is available within the body of with-open-file
;; :output specifies an output stream

;; Specifies an input stream that reads in the file created above
(with-open-file (my-stream "streamData.txt" :direction :input)
		(read my-stream)) 

;; Writes an alist to a file
(let ((animal-noises '((dog . woof) 
		       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
		  (print animal-noises my-stream)))

;; Reads in  the above alist
(with-open-file (my-stream "animal-noises.txt" :direction :input)
		(read my-stream))


;; The with-open-file command can take keyword parameters to modify behavior

;; This example will display an error message using using the :if-exists keyword
;;    error message prints because this file already exists
(with-open-file (my-stream "streamData.txt" :direction :output :if-exists :error)
		(print "my data" my-stream))

#|
 Instead of displaying an error message you may want to overwrite the existing file
 This can be done like this :
|#
(with-open-file (my-stream "streamData.txt" :direction :output
			                    :if-exists :supersede)
		(print "my new data" my-stream))

#| 
The structure of a with-open-file command
(with-open-file (s "data.txt" :direction :output
      ;; s <- file handle is created and stream is bound to it
         (print "my data" s)
	 s <- file handle is destroyed
	 )
|#

;; Working with sockets

#|
Every socket within a network must have a socket address. This socket address has 2 components
   IP address ->  
   Port number -> Any program that wants to use a network must choose a unique port 
                  number that no other program on the same computer is already using
The IP address and the port number combine to make up the socket address
Since the IP address is unique on a network and the port number is unique for a given
computer, every socket address on a network is unique to a specific a specific program 
running on a specific computer
|#

;; On the server, take contol of a port by calling socket-server
(defparameter my-socket (socket-server 4321)) ;; on server
;; this command aquires port 4321 and binds a socket to it using the operating system
;; This can be dangerous review page 246 before implementing 

;; Still on the server, create a stream from the above socket that handles a connection from a single 
;; client 
(defparameter my-stream (socket-accept my-socket)) ;; on server
;; socket-accept is aa blocking operation so the funtion will not exit until a client has connected

;; on the client, use the socket-connect commandto connect to that socket on the server 
(defparameter my-stream (socket-connect 4321 "107.2.184.1"))
