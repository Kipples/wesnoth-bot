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

;; attribute `attribute-key` = `attribute-value`
(defrule attribute (and attribute-key #\= attribute-value newline)
  (:destructure (key assign val newline)
		(declare (ignore assign newline))
		(list :attribute key val)))

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
		(list :attribute key val)))
(defrule wml-or-ping (or wml ping)
  (:lambda (wml-or-ping)
    (if (eql :attribute (car wml-or-ping))
	(acons "ping" (list (process-esrap-wml-attribute wml-or-ping)) '())
	(process-esrap-wml wml-or-ping))))

;; the following functions take the output from the esrap parsing and
;; process it into a alist form e.g.
;; '("top_level_tag" ("nested_tag" ("an_attribute" . 2))

(defun process-esrap-wml (esrap-wml)
  (process-esrap-wml-fix-missing-top-level-tag (mapcar #'process-esrap-wml-tags esrap-wml)))

;; if the processed esrap-wml contains any attributes inside the root node
;; guess which top level tag it needs
(defun process-esrap-wml-fix-missing-top-level-tag (wml)
  (if (not (every #'wml-tag-p wml))
      (process-esrap-wml-add-top-level-tag-heuristic wml)
      wml))

;; right now multiplayer is the only message that doesn't get sent with a top level tag
;; so i fix it here this appears to be fixed for wesnoth verison 1.13 for 1.14
(defun process-esrap-wml-add-top-level-tag-heuristic (wml)
  `(("multiplayer" ,@wml)))

(defun process-esrap-wml-tags (esrap-wml)
  (if (eql :attribute (car esrap-wml))
      (process-esrap-wml-attribute esrap-wml)
      (let ((node (car esrap-wml)))
	(if (eql :open (car node))
	    (cons (cadr node) (process-esrap-wml-tag-contents (cadr esrap-wml)))))))

(defun process-esrap-wml-tag-contents (esrap-wml)
  (mapcar #'process-esrap-wml-attibutes-and-nested-tags esrap-wml))

(defun process-esrap-wml-attibutes-and-nested-tags (esrap-wml)
  (if (eql :attribute (car esrap-wml))
      (process-esrap-wml-attribute esrap-wml)
      (process-esrap-wml-tags esrap-wml)))

(defun process-esrap-wml-attribute (esrap-wml)
  (cons (cadr esrap-wml) (strip-surrounding-double-quotes (caddr esrap-wml))))

(defun strip-surrounding-double-quotes (str)
  (let ((first-dq (position #\" str))
	(last-dq (position #\" str :from-end t)))
    (if (and (eq first-dq 0)
	     (eq last-dq (1- (length str))))
	(subseq str 1 (1- (length str)))
	str)))

(defun wml-tag-p (wml)
  (listp (cdr wml)))

(defun wml-attr-p (wml)
  (not (wml-tag-p wml)))

(defun wml-comment-p (wml)
  (when (wml-attr-p wml)
      (equal "comment" (car wml))))

(defun wml-alist-to-string (wml)
  (format nil "~{~a~%~}" (mapcar #'wml-alist-node-to-string wml)))

(defun wml-alist-node-to-string (wml-node)
  (if (wml-comment-p wml-node)
      (format nil "#~a" (cdr wml-node))
      (if (wml-attr-p wml-node)
	  (format nil "~a=\"~a\"" (car wml-node) (cdr wml-node))
	  (format nil "[~a]~%~{~a~%~}[/~a]"
		  (car wml-node)
		  (mapcar #'wml-alist-node-to-string (cdr wml-node))
		  (car wml-node)))))

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

(defun wml-alist-sort (wml)
  "takes a WML alist and sort it's attributes as this is required for valid WML"
  (wml-sort-tag-contents wml))

(defun wml-sort-tag-contents (wml)
  "sorts all attributes within tag contents"
  (stable-sort wml #'wml-node-compare)
  (mapcar #'(lambda (wml-node)
	      (if (wml-tag-p wml-node)
		  (wml-sort-tag-contents (cdr wml-node))
		  wml-node))
	  wml)
  wml)

(defun wml-node-compare (wml-a wml-b)
  ""
  (when (and (wml-attr-p wml-a)
	     (wml-attr-p wml-b)
	     (not (wml-comment-p wml-a))
	     (not (wml-comment-p wml-b)))
    (string-lessp (car wml-a) (car wml-b))))

(defun encode-wml-to-string (wml)
  "takes a WML alist and converts it to a string that can be sent to the wesnoth server"
  (wml-alist-to-string (wml-alist-sort wml)))

(defun wml-assoc (item wml)
  (assoc item wml :test #'equal))

(defun wml-assoc-all (item wml)
  (remove-if-not #'(lambda (node) (equal item (car node))) wml))
