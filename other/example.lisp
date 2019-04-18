(defpackage #:lem-sucle-example
  (:use :cl))
(in-package :lem-sucle-example)

(setf lem-sucle::*run-sucle* t)
(lem-paredit-mode:paredit-mode)

(dotimes (x 100)
  (print x))