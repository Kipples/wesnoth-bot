;;;; wesnoth-bot.asd

(asdf:defsystem #:wesnoth-bot
  :description "library for writing wesnoth bots"
  :author "Kipples <kipples@kipples.net>"
  :license "GPL"
  :serial t
  :depends-on (:usocket :chipz :flexi-streams :salza2 :esrap :cl-ppcre :bordeaux-threads :md5)
  :components ((:file "package")
	       (:file "wesnoth-connection")
	       (:file "wesnoth-hash")
	       (:file "wesnoth-wml")
               (:file "wesnoth-bot")))
