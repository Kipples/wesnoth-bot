(in-package #:wesnoth-bot)

;; TODO: change to server.wesnoth.org on release
(defvar *wesnoth-connection-default-host* "server.wesnoth.org")
(defvar *wesnoth-connection-default-port* 15000)
(defvar *wesnoth-connection-default-timeout* 30)

;; class to handle the low level wesnoth connection
;; includes performing handshake and methods for reading and writing wesnoth data packets
;; All packages are in gzip format and are preceded by four bytes that specify the size
;; of the package to come in big-endian.
;; unzipped data contains wml forms.

;; this only deals with reading and writing strings
;; the wml parsing is handled eleswhere.

;; helper functions

(defun %make-wesnoth-integer-array ()
  "creates a size 4 array of type UNSIGNED-BYTE as wesnoth uses 32 bit
integers for socket number and data size"
  (make-array 4 :element-type '(unsigned-byte 8)))

(defun %decode-wesnoth-integer (bytes)
  "converts wesnoth 32 bit big-endian integer to lisp
BYTES is a 4 byte array containing the 32 bit integer"
  (let ((result 0))
    (loop for b across (reverse bytes)
	  for ix from 0 do
	    (setf result (logior result (ash b (* ix 8)))))
    result))

(defun %get-lsb-byte (number byte)
  (logand #xff (ash number (* byte -8))))

(defun %encode-wesnoth-integer (number)
  "encodes NUMBER into a wesnoth 32 bit integer returns an 4 byte array containing the NUMBER"
  (let ((result (%make-wesnoth-integer-array))) 
   (loop for x from 0 below 4
	  do (setf (aref result x) (%get-lsb-byte number x)))
   (reverse result)))

(defun %read-wesnoth-integer (stream)
  "read and decode a single wesnth integer from STREAM"
  (let ((data (%make-wesnoth-integer-array)))
    (read-sequence data stream :end 4)
    (%decode-wesnoth-integer data)))

;; class definition

(defclass wesnoth-connection ()
  ((usocket-connection :initarg :usocket-connection
		       :initform (usocket:socket-connect
				 *wesnoth-connection-default-host*
				 *wesnoth-connection-default-port*
				 :protocol :stream
				 :element-type '(unsigned-byte 8)
				 :timeout *wesnoth-connection-default-timeout*)
		       :reader usocket-connection)
   (socket-number :reader socket-number)))

;; helper methods

(defmethod %socket-stream ((wc wesnoth-connection))
  "returns the STREAM associated with this wesnoth connection"
  (usocket:socket-stream (usocket-connection wc)))

(defmethod %read-socket-number ((wc wesnoth-connection))
  "reads the SOCKET-NUMBER from the wesnoth connection.
SOCKET-NUMBER is read in and decoded from a 32 bit big-endian integer."
  (%read-wesnoth-integer (%socket-stream wc)))

(defmethod %write-wesnoth-handshake ((wc wesnoth-connection))
  "write the wesnoth handshake a 4 byte array of zeros to the wesnoth connection"
  (let ((data (%make-wesnoth-integer-array)))
    (write-sequence data (%socket-stream wc) :end 4)
    (force-output (%socket-stream wc))))

;; interface

(defmethod initialize-instance :after ((wc wesnoth-connection) &key &allow-other-keys)
  "performs the wesnoth handshake and initializes the SOCKET-NUMBER puts connection into state to begin reading wesnoth data."
  (%write-wesnoth-handshake wc)
  (with-slots (socket-number) wc
    (setf socket-number (%read-socket-number wc))))

(defmethod read-wesnoth-wml-string ((wc wesnoth-connection))
  "reads and returns a string containing wesnoth wml forms."
  (let* ((stream (%socket-stream wc))	 
	 (gzip-size (%read-wesnoth-integer stream))
	 (gzip-data (make-array gzip-size :element-type '(unsigned-byte 8)))
	 (output-stream (flexi-streams:make-in-memory-output-stream)))
    (read-sequence gzip-data stream :end gzip-size)
    (chipz:decompress output-stream 'chipz:gzip gzip-data)
    (flexi-streams:octets-to-string
     (flexi-streams:get-output-stream-sequence output-stream)
     :external-format :utf-8)))

(defmethod write-wesnoth-wml-string ((wc wesnoth-connection) wml-string)
  "write WML-STRING to wesnoth connection stream"
  (let ((stream (%socket-stream wc))
	(gzip-data
	 (salza2:compress-data (flexi-streams:string-to-octets wml-string :external-format :utf-8)
			       'salza2:gzip-compressor)))
    (write-sequence (%encode-wesnoth-integer (length gzip-data))
		    stream)
    (write-sequence gzip-data stream)
    (force-output stream)))

(defmethod close-connection ((wc wesnoth-connection))
  "closes the wesnoth connection"
  (usocket:socket-close (usocket-connection wc)))

(defmethod redirect-connection ((wc wesnoth-connection) host port)
  "redirects the wesnoth connection to HOST:PORT"
  (close-connection wc)
  (with-slots (usocket-connection socket-number) wc
    (setf usocket-connection (usocket:socket-connect
			      host port
			      :protocol :stream
			      :element-type '(unsigned-byte 8)
			      :timeout *wesnoth-connection-default-timeout*))
    (%write-wesnoth-handshake wc)
    (setf socket-number (%read-socket-number wc))))
