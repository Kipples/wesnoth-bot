(in-package #:wesnoth-bot)

(defparameter +hash-prefix+ "$H$")

(defparameter *wesnoth-encode-table* "./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(defgeneric %wesnoth-md5 (input))

(defmethod %wesnoth-md5 ((input string))
  (md5sum-string input))

(defmethod %wesnoth-md5 ((input sequence))
  (md5sum-sequence input))

(defun %wesnoth-md5-get-salt-low (salt)
  (subseq salt 4 12))
(defun %wesnoth-md5-get-salt-high (salt)
  (subseq salt 12 20))

(defun wesnoth-md5 (password salt &optional (iteration-count 10))
  (let ((iteration-count (ash 1 iteration-count))
	(hash (%wesnoth-md5 (concatenate 'string salt password))))
    (dotimes (i iteration-count hash)
      (setq hash  (%wesnoth-md5
		   (flexi-streams:string-to-octets
		    (concatenate 'string
				 (flexi-streams:octets-to-string hash)
				 password)))))))

(defun wesnoth-password-encode (password salt)
  (%wesnoth-encode-hash
   (wesnoth-md5
    (%wesnoth-encode-hash
     (wesnoth-md5 password
		  (%wesnoth-md5-get-salt-low salt)
		  (%wesnoth-md5-iteration-count salt)))
    (%wesnoth-md5-get-salt-high salt))))

(defun %wesnoth-md5-iteration-count (hash)
  (position (elt hash 3) *wesnoth-encode-table*))

(defun cat (str1 str2)
  (concatenate 'string str1 str2))

(defun %wesnoth-encode-table-lookup (n)
  (let ((n (logand n #x3f)))
    (subseq *wesnoth-encode-table* n (+ 1 n))))

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
