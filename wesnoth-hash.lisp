(in-package #:wesnoth-bot)

;; overview of wesnoth login protocol with password:
;; 1. you attempt to log in with username.
;; 2. server responds with error telling you to provide password.
;;    error structure contains a salt you will use to encode your password.
;; 3. perform the first round if iterative md5 hashing.
;;    you derive the number of iterations from the salt.
;;    for the first round you use the low 8 chars from the salt.
;; 4. base64 encode the result of above.
;; 5. perform the second round of iterative md5 hashing using above base64 encoded data.
;;    the second round always has the same iteration count of 10.
;;    for the second round you use the high 8 chars from the salt.
;; 6. base64 encode the result of above.
;; 7. send the result with "password" set to above with the normal login request.

;; this table is used to base64 encode arrays to be sent over the network
(defparameter *wesnoth-encode-table* "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defgeneric %wesnoth-md5 (input))

(defmethod %wesnoth-md5 ((input string))
  (md5sum-string input))

(defmethod %wesnoth-md5 ((input sequence))
  (md5sum-sequence input))

;; functions to retrieve the relevant parts of the salt
;; the server sends the salt with "$H$" prefixed so we skip over it
(defun %wesnoth-md5-get-salt-low (salt)
  (subseq salt 4 12))
(defun %wesnoth-md5-get-salt-high (salt)
  (subseq salt 12 20))

;; this is where the iterative md5sum happens
;; everything gets hashed 2 to the power iteration count times plus 1
;; each iteration you concatenate the result of the previous iteration with the password
(defun wesnoth-md5 (password salt &optional (iteration-count 10))
  (let ((iteration-count (ash 1 iteration-count))
	(hash (%wesnoth-md5 (concatenate 'string salt password))))
    (dotimes (i iteration-count hash)
      (setq hash  (%wesnoth-md5
		   (flexi-streams:string-to-octets
		    (concatenate 'string
				 (flexi-streams:octets-to-string hash)
				 password)))))))

;; password encoding happens in 2 rounds
;; each round is base64 encoded
(defun wesnoth-password-encode (password salt)
  (%wesnoth-encode-hash
   (wesnoth-md5
    (%wesnoth-encode-hash
     (wesnoth-md5 password
		  (%wesnoth-md5-get-salt-low salt)
		  (%wesnoth-md5-iteration-count salt)))
    (%wesnoth-md5-get-salt-high salt))))

;; retrieves the iteration count from the salt for the first round of md5suming
(defun %wesnoth-md5-iteration-count (hash)
  (position (elt hash 3) *wesnoth-encode-table*))

(defun cat (str1 str2)
  (concatenate 'string str1 str2))

(defun %wesnoth-encode-table-lookup (n)
  (let ((n (logand n #x3f)))
    (subseq *wesnoth-encode-table* n (+ 1 n))))

;; performs wesnoth's base64 encoding on the input
(defun %wesnoth-encode-hash (input)
  (let ((encoded-hash "")
	(len (length input))
	(i 0))
    (loop while (< i len) do
	 (let ((value (elt input i)))
	   (incf i)
	   (setq encoded-hash (cat encoded-hash (%wesnoth-encode-table-lookup value)))
	   (when (< i len)
	     (setq value (logior value (ash (elt input i) 8))))
	   (setq encoded-hash (cat encoded-hash (%wesnoth-encode-table-lookup (ash value -6))))
	   (incf i)
	   (when (>= i len)
	     (return))
	   (when (< i len)
	     (setq value (logior value (ash (elt input i) 16))))
	   (setq encoded-hash (cat encoded-hash (%wesnoth-encode-table-lookup (ash value -12))))
	   (incf i)
	   (when (>= i len)
	     (return))
	   (setq encoded-hash (cat encoded-hash (%wesnoth-encode-table-lookup (ash value -18))))))
    encoded-hash))
