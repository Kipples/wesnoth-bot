;;;; wesnoth-bot.lisp

(in-package #:wesnoth-bot)

;;; "wesnoth-bot" goes here. Hacks and glory await!

(defvar *current-bot* nil)

(defclass wesnoth-bot ()
  ((username :initarg :username
	     :initform (error "must provide :username to wesnoth-bot")
	     :reader wesnoth-bot-username)
   (password :initarg :password
	     :initform nil
	     :reader wesnoth-bot-password)
   (version :initarg :version
	    :initform (error "must provide :version to wesnoth-bot")
	    :reader wesnoth-bot-version)
   (wesnoth-connection :reader %wesnoth-connection)
   (%runningp :initform nil
	      :accessor %runningp)
   (commands :initform nil
	     :initarg :commands
	     :accessor wesnoth-bot-commands)
   (db :initform '())))

(defun match-wesnoth-bot-command (command command-name)
  (equal command-name
	 (concatenate 'string
		      (wesnoth-bot-command-prefix command)
		      (wesnoth-bot-command-name command))))

(defgeneric dispatch-wesnoth-command (wesnoth-bot command-string)
  (:method ((wesnoth-bot wesnoth-bot) command-string)
    (let* ((command-split (remove "" (ppcre:split "\\s+" command-string) :test #'equal))
	   (command-name (car command-split))
	   (command-params (cdr command-split))
	   (command (car (member-if (lambda (command)
				      (match-wesnoth-bot-command command command-name))
				    (wesnoth-bot-commands wesnoth-bot)))))
      (when command
	  (funcall (wesnoth-bot-command-handler command) command-params)))))

(defmethod %write-wml ((bot wesnoth-bot) wml)
  (write-wesnoth-wml-string (%wesnoth-connection bot) (serialize-wml-node wml)))

(defmethod %read-wml ((bot wesnoth-bot))
  (when (%runningp bot)
    (let ((string (read-wesnoth-wml-string (%wesnoth-connection bot))))
      (parse-wml-string string))))

(defmethod %write-wml-version ((bot wesnoth-bot))
  (%write-wml bot (parse-wml-string (format nil "[version]~%version=\"~a\"~%[/version]~%"
					    (wesnoth-bot-version bot)))))

(defmethod %write-wml-login ((bot wesnoth-bot) &optional (password))
  (%write-wml bot
	      (parse-wml-string
	       (format nil "[login]~%username=\"~a\"~%password_reminder=no~%~a[/login]~%"
		       (wesnoth-bot-username bot)
		       (if password
			   (format nil "password=~a~%" password)
			   "")))))

(defmethod %handle-server-response ((bot wesnoth-bot) wml)
  (cond
    ((wml-node-find-child-by-name wml "reject") (handle-reject bot wml))
    ((wml-node-find-child-by-name wml "redirect") (handle-redirect bot wml))
    ((wml-node-find-child-by-name wml "version") (handle-version bot wml))
    ((wml-node-find-child-by-name wml "mustlogin") (handle-mustlogin bot wml))
    ((wml-node-find-child-by-name wml "join_lobby") (handle-join-lobby bot wml))
    ((wml-node-find-child-by-name wml "error") (handle-error bot wml))
    ((and (wml-node-find-child-by-name wml "gamelist")
	  (wml-node-find-child-by-name wml "user"))
     (handle-lobby-data bot wml))
    ((wml-node-find-child-by-name wml "message") (handle-message bot wml))
    ((wml-node-find-child-by-name wml "whisper") (handle-whisper bot wml))
    ((wml-node-find-child-by-name wml "gamelist_diff") (handle-lobby-data-diff bot wml))
    ((wml-node-attribute wml "ping") (handle-ping bot wml))
    ((wml-node-find-child-by-name wml "scenario_diff") (handle-scenario-diff bot wml))
    ((wml-node-find-child-by-name wml "multiplayer") (handle-multiplayer bot wml))
    ((wml-node-find-child-by-name wml "leave_game") (handle-leave-game bot wml))
    ((wml-node-find-child-by-name wml "stop_updates") (handle-stop-updates bot wml))
    ((wml-node-find-child-by-name wml "change_controller") (handle-change-controller bot wml))
    ((wml-node-find-child-by-name wml "start_game") (handle-start-game bot wml))
    ((wml-node-find-child-by-name wml "turn") (handle-turn bot wml))
    ((wml-node-find-child-by-name wml "host_transfer") (handle-host-transfer bot wml))
    ((wml-node-find-child-by-name wml "observer") (handle-observer bot wml))
    ((wml-node-find-child-by-name wml "observer_quit") (handle-observer-quit bot wml))
    (t (error "bot does not know how to handle ~a." (serialize-wml-node wml)))))

(defmethod initialize-instance :after ((bot wesnoth-bot) &key &allow-other-keys)
  (with-slots (wesnoth-connection) bot
    (setf wesnoth-connection (make-instance 'wesnoth-connection))))

(defmethod handle-reject ((bot wesnoth-bot) wml)
  (error "server version: ~a does not match bot version: ~a"
	 "FIXMEKIPPLES"
	 (wesnoth-bot-version bot)))

(defmethod handle-redirect ((bot wesnoth-bot) wml)
  (let* ((redirect (wml-node-find-child-by-name wml "redirect"))
	 (host (wml-node-attribute redirect "host"))
	 (port (wml-node-attribute redirect "port")))
    (redirect-connection (%wesnoth-connection bot) host (parse-integer port))))

(defmethod handle-version ((bot wesnoth-bot) wml)
  (%write-wml-version bot))

(defmethod handle-mustlogin ((bot wesnoth-bot) wml)
  (%write-wml-login bot))

(defmethod handle-join-lobby ((bot wesnoth-bot) wml))

(defmethod handle-lobby-data ((bot wesnoth-bot) wml))

(defmethod handle-message ((bot wesnoth-bot) wml)
  (let ((message (attribute-value (wml-node-attribute (wml-node-find-child-by-name wml "message") "message"))))
    (let ((*current-bot* bot))
      (dispatch-wesnoth-command bot message))))

(defmethod handle-whisper ((bot wesnoth-bot) wml)
  (let ((message (attribute-value (wml-node-attribute (wml-node-find-child-by-name wml "whisper") "message"))))
    (let ((*current-bot* bot))
      (dispatch-wesnoth-command bot message))))

(defmethod handle-lobby-data-diff ((bot wesnoth-bot) wml))

(defmethod handle-ping ((bot wesnoth-bot) wml))

(defmethod disconnect-bot ((bot wesnoth-bot))
  (close-connection (%wesnoth-connection bot)))

(defmethod handle-error ((bot wesnoth-bot) wml)
  (let ((error-code (parse-integer (cdr (wml-assoc "error_code" (cdr (wml-assoc "error" wml)))))))
    (case error-code
      (200 (handle-password-challenge bot wml))
      (otherwise (error "wesnoth server sent error: ~a~%" wml)))))

(defmethod handle-lobby-data ((bot wesnoth-bot) wml))

(defmethod handle-scenario-diff ((bot wesnoth-bot) wml))

(defmethod handle-multiplayer ((bot wesnoth-bot) wml))

(defmethod handle-leave-game ((bot wesnoth-bot) wml))

(defmethod handle-stop-updates ((bot wesnoth-bot) wml))

(defmethod handle-change-controller ((bot wesnoth-bot) wml))

(defmethod handle-start-game ((bot wesnoth-bot) wml))

(defmethod handle-turn ((bot wesnoth-bot) wml))

(defmethod handle-host-transfer ((bot wesnoth-bot) wml))

(defmethod handle-observer ((bot wesnoth-bot) wml))

(defmethod handle-observer-quit ((bot wesnoth-bot) wml))

(defmethod handle-password-challenge ((bot wesnoth-bot) wml)
  (let* ((salt (cdr (wml-assoc "salt" (cdr (wml-assoc "error" wml)))))
	 (password (wesnoth-password-encode (wesnoth-bot-password bot) salt)))
    (%write-wml-login bot password)))

(defmethod start-bot ((bot wesnoth-bot))
  (setf (%runningp bot) t)
  (unwind-protect
       (loop for wml = (%read-wml bot)
	  while (%runningp bot) do
	    (when wml
	      (format t "wml:~%~a~%" wml)
	      (%handle-server-response bot wml)))
    (stop-bot *current-bot*)))

(defmethod stop-bot ((bot wesnoth-bot))
  (setf (%runningp bot) nil)
  (disconnect-bot bot))

(defmethod wml-message ((bot wesnoth-bot) message &optional (sender) (room))
  (%write-wml
   bot
   (parse-wml-string (format nil "[message]~%message=\"~a\"~%[/message]~%" message))))

(defmethod wml-whisper ((bot wesnoth-bot) receiver message &optional (sender))
  (%write-wml
   bot
   (parse-wml-string (format nil "[whisper]~%receiver=\"~a\"~%message=\"~a\"~%[/whisper]~%" receiver message))))

(defvar *default-command-prefix* "!")

(defstruct wesnoth-bot-command
  name
  doc-string
  prefix
  handler)

(defmacro define-wesnoth-command (name command-prefix name-string (lambda-list) doc-string &body body)
  `(progn
     (defparameter ,name
       (make-wesnoth-bot-command
	:name ,name-string
	:doc-string ,doc-string
	:prefix ,command-prefix
	:handler (lambda (,lambda-list)
		   ,@body)))))

(defmacro define-wesnoth-bot-spawner (name version (&rest commands) &body body)
  `(progn
     (defun ,(intern (format nil "MAKE-~a" (string name))) (username &optional (password))
       (bt:make-thread (lambda () 
			 (let ((*current-bot* (make-instance
					       'wesnoth-bot
					       :commands (list ,@commands)
					       :version ,version
					       :username username
					       :password password)))
			   ,@body))
		       :name (string username)
		       :initial-bindings (acons 'password password
						(acons 'username username bt:*default-special-bindings*))))))
