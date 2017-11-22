;;;; package.lisp

(defpackage #:wesnoth-bot
  (:use #:cl #:esrap #:md5)
  (:export
   #:*current-bot*

   #:*default-command-prefix*
   
   #:define-wesnoth-command
   #:define-wesnoth-bot-spawner

   #:start-bot
   #:stop-bot

   #:wml-message
   #:wml-whisper))

