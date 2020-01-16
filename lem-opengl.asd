(defsystem "lem-opengl"
  :depends-on ("cffi"
	       ;;"cl-charms"
	       "control"
	       "lparallel"
               "trivial-clipboard"
               ;;#+(or (and ccl unix) (and lispworks unix))"lem-setlocale"
               "minilem"

	       #:cl-ppcre
	       #:application
	       #:utility
	       #:text-subsystem
	       ;;#:opengl-immediate
	       ;;#:character-modifier-bits
	       #:uncommon-lisp
	       #:livesupport

	       #:sucle)
  :serial t
  :components ((:file "package")
	       (:file "ncurses-clone")
               (:file "term")
	       (:file "ncurses-clone-lem-view")
	       
	       (:file "impl")
	       (:file "keys")
	       (:file "app")
               (:file "sucle")
	       (:module "other"
			:components
			((:file "test")
			 (:file "test2")))))
