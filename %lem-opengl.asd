(asdf:defsystem #:%lem-opengl
  :depends-on (#:application
	       #:utility
	       #:text-subsystem
	       ;;#:opengl-immediate
	       #:character-modifier-bits
	       #:uncommon-lisp)
  :serial t
  :components 
  ((:file "ncurses-clone")
   (:file "%lem-opengl")))
