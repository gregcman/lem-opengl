(defpackage #:lem-sucle-example
  (:use :cl))
(in-package :lem-sucle-example)

(setf lem-sucle::*run-sucle* t)
(lem-paredit-mode:paredit-mode)

(dotimes (x 100)
  (print x))

(defparameter *an-overlay*
  (lem:make-overlay
   (lem:current-point)
   (lem:save-excursion
     (lem:forward-char 10)
     (lem:copy-point (lem:current-point) :temporary))
   'lem:modeline))

(lem:delete-overlay *an-overlay*)
(defun remove-empty-overlay (&optional (overlay *an-overlay*))
  (when (lem:point=
         (lem:overlay-start overlay)
         (lem:overlay-end overlay))
    (print "removing overlay")
    (lem:delete-overlay overlay)))
(remove-empty-overlay)
;;TODO:subclass lem:attribute in order to attach extra data to attributes
;;have one attribute per overlay?