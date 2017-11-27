(in-package #:wesnoth-bot)

;; here is some esrap parsing rules to parse wesnoth wml

;; wesnoth symbol not entirely sure if this is all that is allowed here
(defrule symbol (+ (or
		    (character-ranges (#\a #\z))
		    (character-ranges (#\A #\Z))
		    #\_))
  (:text t))

(defrule number (+ (and
		    (? #\-)
		    (? #\+)
		    (character-ranges (#\0 #\9))))
  (:text t))

;; whitespace 
(defrule ws (* (or #\space #\tab )))
(defrule newline (+ #\newline))

;; wesnoth open tag [`symbol`]
(defrule open-tag (and ws #\[ symbol #\] newline)
  (:function third)
  (:around () (list :open (call-transform))))

;; wesnoth close tag [/`symbol]
(defrule close-tag (and ws #\[ #\/ symbol #\] newline)
  (:function fourth)
  (:around () (list :close (call-transform))))

(defun not-doublequote (char)
  "returns T if CHAR is not a doublequote"
  (not (eql char #\")))

;; characters allowed inside a doublquoted string
(defrule string-char (not-doublequote character))

;; wesnoth attribute `attribute-key` = `attribute-value`
;; attribute key `symbol`
(defrule attribute-key (and ws symbol ws)
  (:function second))
;; attribute value "`string-char`" or `symbol`
(defrule attribute-value (and ws
			      (or
			       attribute-string-concatenation
			       attribute-translatable-value
			       attribute-string
			       symbol
			       number)
			      ws)
  (:function second)
  (:text t))

(defrule attribute-string (and #\" (* (or string-char (and #\" #\"))) #\")
  (:text t))

(defrule attribute-translatable-value (and ws #\_ ws attribute-string)
  (:function fourth)
  (:text t))

(defrule attribute-string-value (or attribute-string attribute-translatable-value))

(defrule attribute-string-concatenation (and attribute-string-value (* (and ws #\+ ws (? newline) (? comment) ws (? newline) attribute-string-value)))
  (:destructure (first-string rest-strings)
		(apply #'concatenate 'string (mapcar #'strip-surrounding-double-quotes
						     (cons first-string 
							   (mapcar (lambda (thing)
								     (eighth thing))
								   rest-strings))))))

(defrule text-domain (and ws "#textdomain " ws (* (not #\Newline)) newline)
  (:function fourth)
  (:text t))

;; attribute `attribute-key` = `attribute-value`
(defrule attribute (and (? text-domain) attribute-key #\= attribute-value newline)
  (:destructure (text-domain key assign val newline)
		(declare (ignore assign newline))
		(list :attribute key val text-domain)))

(defrule comment (and ws #\# (* (not #\Newline)) newline)
  (:destructure (ws hash comment nl)
		(declare (ignore ws hash nl))
		(list :attribute "comment" (text comment))))

;; wml consists of top level tags with optional tag contents
(defrule wml (+ (or (and open-tag tag-contents close-tag) attribute comment)))

;; wml tag contents contain attributes and/or nested wml
(defrule tag-contents (* (or (and open-tag tag-contents close-tag) attribute comment)))

;; degenerate case where the server sends just an attribute `ping` with no top level tags
;; this will process the ping into a wml structure with top level tag 'ping' to contain attribute
(defrule ping (and attribute-key #\= attribute-value)
  (:destructure (key assign val)
		(declare (ignore assign))
		(list (list :attribute key val))))
(defrule wml-or-ping (or wml ping)
  (:function esrap->wml-node))

;; interface

(defun parse-wml-string (wml-string)
  "takes a WML-STRING and converts it into a alist format"
  (parse 'wml-or-ping wml-string))

(defun parse-wml-from-file (file)
  (with-open-file (f file)
    (parse-wml-string
     (format nil "~{~A~%~}" (loop for line = (read-line f nil 'eof)
			       until (eq line 'eof)
			       collect line)))))
