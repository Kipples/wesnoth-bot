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

(defclass wml-node ()
  ((name :initarg :name :initform nil :reader wml-node-name)
   (parent :initform nil :reader wml-node-parent)
   (attributes :initform nil :accessor %wml-node-attributes)
   (children :initform nil :accessor %wml-node-children)
   (last-child :initform nil :accessor wml-node-last-child)))

(defstruct attribute
  value
  text-domain)

(defun wml-node-assoc (item alist)
  (assoc item alist :test #'equal))

(defmethod (setf %wml-node-children) :after (value (node wml-node))
  (setf (wml-node-last-child node) (last value)))

(defmethod wml-node-append-child ((node wml-node) child)
  (when (wml-node-parent child)
    (wml-node-remove-child (wml-node-parent child) child))
  (setf (slot-value child 'parent) node)
  (if (%wml-node-children node)
      (setf (wml-node-last-child node)
	    (push child (cdr (wml-node-last-child node))))
      (setf (%wml-node-children node) (list child)))
  (%wml-node-children node))

(defmethod wml-node-remove-child ((node wml-node) child)
  (setf (%wml-node-children node)
	(remove child (%wml-node-children node)))
  (setf (slot-value child 'parent) nil))

(defmethod wml-node-attribute ((node wml-node) key)
  (cdr (wml-node-assoc key (%wml-node-attributes node))))

(defmethod (setf wml-node-attribute) (new-value (node wml-node) key)
  (let ((old-attribute (wml-node-assoc key (%wml-node-attributes node))))
    (if old-attribute
	(setf (attribute-value (cdr old-attribute)) new-value)
	(push (cons key (make-attribute :value new-value))
	      (%wml-node-attributes node)))))

(defmethod (setf wml-node-attribute-text-domain) (new-value (node wml-node) key)
  (let ((old-attribute (wml-node-assoc key (%wml-node-attributes node))))
    (if old-attribute
	(setf (attribute-text-domain (cdr old-attribute)) new-value)
	(error "can not set attribute's text domain if it hasn't already been created"))))

(defmethod insert-wml-node-attribute ((node wml-node) key value &optional text-domain)
  (let ((attribute (make-attribute :value value :text-domain text-domain)))
    (push (cons key attribute) (%wml-node-attributes node))))

(defmethod wml-node-find-child-by-name ((node wml-node) child-name)
  (loop for child in (%wml-node-children node) do
       (when (equal child-name
		    (wml-node-name child))
	 (return child))))

(defun esrap->wml-node (wml &optional name)
  (let ((node (make-instance 'wml-node :name name)))
    (loop for element in wml do
	 (cond ((eql :attribute (car element))
		(insert-wml-node-attribute node
					   (cadr element)
					   (caddr element)
					   (cadddr element)))

	       ((eql :open (caar element))
		(wml-node-append-child
		 node
		 (esrap->wml-node (cadr element) (cadar element))))))
    node))

(defmethod serialize-wml-node ((node wml-node))
  (format nil "~a~{~a~}~{~a~}~a"
	  (wml-node-name-open-tag (wml-node-name node))
	  (mapcar #'serialize-wml-node-attribute
		  (sort 
		   (%wml-node-attributes node)
		   (lambda (a1 a2)
		     (string-lessp (car a1) (car a2)))))
	  (mapcar #'serialize-wml-node (%wml-node-children node))
	  (wml-node-name-close-tag (wml-node-name node))))

(defun wml-node-name-open-tag (name)
  (if name
      (format nil "[~a]~%" name)
      ""))

(defun wml-node-name-close-tag (name)
  (if name
      (format nil "[/~a]~%" name)
      ""))

(defun serialize-wml-node-attribute (attribute)
  (if (attribute-text-domain (cdr attribute))
      (format nil "#textdomain ~a~%~a=\"~a\"~%"
	      (attribute-text-domain (cdr attribute))
	      (car attribute)
	      (attribute-value (cdr attribute)))
      (format nil "~a=\"~a\"~%"
	      (car attribute)
	      (attribute-value (cdr attribute)))))
