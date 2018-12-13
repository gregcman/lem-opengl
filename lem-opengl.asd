(defsystem "lem-opengl"
  :depends-on ("cffi"
	       ;;"cl-charms"
	       "control"
	       "lparallel"
               "trivial-clipboard"
               #+(or (and ccl unix) (and lispworks unix))"lem-setlocale"
	       "%lem-opengl"
               "lem")
  :serial t
  :components (
               (:file "term")
               (:file "sucle")))
