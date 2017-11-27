(in-package #:wesnoth-bot)

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
