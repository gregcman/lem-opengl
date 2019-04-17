(in-package :lem-sucle)
(defun start-lem ()
  (let ((lem::*in-the-editor* nil))
    ;;(lem:main nil)
    ;;#+nil
    (progn
      (lem:define-key lem:*global-keymap* "C-/" 'lem:undo)
      (lem:lem "/home/imac/Documents/common-lisp/sucle.lisp")
      (lem:send-event (lambda () (lem-paredit-mode:paredit-mode))))
    (lem-sucle::input-loop)))

(defparameter *packages* nil)
(defun find-lem-package ()
  (remove-if-not (lambda (x)
		   (prefix-p "LEM" 
			     (package-name x)))
		 (list-all-packages)))
;;;FIXME::see cepl.examples/cleanup for similar code
(setf *packages* (find-lem-package))
(defun find-variables (&optional (packages *packages*))
  (let ((acc nil))
    (dolist (package packages)
      (do-symbols (sym package)
	(when (boundp sym)
	  (when (eq (symbol-package sym)
		    package)
	    (push sym acc)))))
    acc))

(defun prefix-p (prefix string)
  "test whether prefix is a prefix of string"
  (let ((len (length prefix)))
    (search prefix string
	    :start1 0 :end1 len
	    :start2 0 :end2 (min len (length string)))))
