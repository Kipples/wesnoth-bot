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
  (write-wesnoth-wml-string (%wesnoth-connection bot) (encode-wml-to-string wml)))

(defmethod %read-wml ((bot wesnoth-bot))
  (when (%runningp bot)
    (let ((string (read-wesnoth-wml-string (%wesnoth-connection bot))))
      ;;    (format t "raw wml-string: ~a~%" string)
      (parse-wml-string string))))

(defmethod %write-wml-version ((bot wesnoth-bot))
  (%write-wml bot `(("version" ("version" . ,(wesnoth-bot-version bot))))))

(defmethod %write-wml-login ((bot wesnoth-bot) &optional (password))
  (%write-wml bot `(("login" ("username" . ,(wesnoth-bot-username bot))
			     ("password_reminder" . "no")
			     ,@(when password
				 `(("password" . ,password)))))))

(defmethod %handle-server-response ((bot wesnoth-bot) wml)
  (cond
    ((wml-assoc "reject" wml) (handle-reject bot wml))
    ((wml-assoc "redirect" wml) (handle-redirect bot wml))
    ((wml-assoc "version" wml) (handle-version bot wml))
    ((wml-assoc "mustlogin" wml) (handle-mustlogin bot wml))
    ((wml-assoc "join_lobby" wml) (handle-join-lobby bot wml))
    ((wml-assoc "error" wml) (handle-error bot wml))
    ((and (wml-assoc "gamelist" wml) (wml-assoc "user" wml)) (handle-lobby-data bot wml))
    ((wml-assoc "message" wml) (handle-message bot wml))
    ((wml-assoc "whisper" wml) (handle-whisper bot wml))
    ((wml-assoc "gamelist_diff" wml) (handle-lobby-data-diff bot wml))
    ((wml-assoc "ping" wml) (handle-ping bot wml))
    ((wml-assoc "scenario_diff" wml) (handle-scenario-diff bot wml))
    ((wml-assoc "multiplayer" wml) (handle-multiplayer bot wml))
    ((wml-assoc "leave_game" wml) (handle-leave-game bot wml))
    ((wml-assoc "stop_updates" wml) (handle-stop-updates bot wml))
    ((wml-assoc "change_controller" wml) (handle-change-controller bot wml))
    ((wml-assoc "start_game" wml) (handle-start-game bot wml))
    ((wml-assoc "turn" wml) (handle-turn bot wml))
    ((wml-assoc "host_transfer" wml) (handle-host-transfer bot wml))
    ((wml-assoc "observer" wml) (handle-observer bot wml))
    ((wml-assoc "observer_quit" wml) (handle-observer-quit bot wml))
    (t (error "bot does not know how to handle ~a." wml))))

(defmethod initialize-instance :after ((bot wesnoth-bot) &key &allow-other-keys)
  (with-slots (wesnoth-connection) bot
    (setf wesnoth-connection (make-instance 'wesnoth-connection))))

(defmethod handle-reject ((bot wesnoth-bot) wml)
  (error "server version: ~a does not match bot version: ~a"
	 (wml-assoc "accepted_versions" (wml-assoc "reject" wml))
	 (wesnoth-bot-version bot)))

(defmethod handle-redirect ((bot wesnoth-bot) wml)
  (let ((host (cdr (wml-assoc "host" (cdr (wml-assoc "redirect" wml)))))
	(port (cdr (wml-assoc "port" (cdr (wml-assoc "redirect" wml))))))
    (redirect-connection (%wesnoth-connection bot) host (parse-integer port))))

(defmethod handle-version ((bot wesnoth-bot) wml)
  (%write-wml-version bot))

(defmethod handle-mustlogin ((bot wesnoth-bot) wml)
  (%write-wml-login bot))

(defmethod handle-join-lobby ((bot wesnoth-bot) wml))

(defmethod handle-lobby-data ((bot wesnoth-bot) wml))

(defmethod handle-message ((bot wesnoth-bot) wml)
  (let ((message (cdr (wml-assoc "message" (cdr (wml-assoc "message" wml))))))
    (let ((*current-bot* bot))
      (dispatch-wesnoth-command bot message))))

(defmethod handle-whisper ((bot wesnoth-bot) wml)
  (let ((message (cdr (wml-assoc "message" (cdr (wml-assoc "whisper" wml))))))
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
   `(("message" ("message" . ,message)
		,@(when sender
		    `(("sender" . ,sender)))
		,@(when room
		    `(("room" . ,room)))))))

(defmethod wml-whisper ((bot wesnoth-bot) receiver message &optional (sender))
  `(("whisper" ("message" . ,message)
	       ("receiver" . ,receiver)
	       ,@ (when sender
		    `(("sender" . ,sender))))))

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

(define-wesnoth-command help *default-command-prefix* "help" (params)
    "displays help message use !help command for command specific help"
  (let ((help-command (car (member-if (lambda (command)
				      (match-wesnoth-bot-command command (car params)))
				    (wesnoth-bot-commands *current-bot*)))))
    (wml-message
     *current-bot*
     (if help-command
	 (wesnoth-bot-command-doc-string help-command)
	 "displays help message use !help command for command specific help"))))

(define-wesnoth-command quit *default-command-prefix* "quit" (params)
    "stops the bot quitting it from the server"
  (stop-bot *current-bot*))


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

(define-wesnoth-bot-spawner boring-bot "1.12.6" (help quit)
  (start-bot *current-bot*))
