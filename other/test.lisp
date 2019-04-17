(in-package :lem-sucle)
(lem:add-hook
 lem:*before-init-hook*
 (lambda ()
   (lem:load-theme "misterioso")))

(defun start-lem ()
  (let ((lem::*in-the-editor* nil))
    ;;(lem:main nil)
    ;;#+nil
    (setf lem.term::*ansi-color-names-vector*
	  ;;from misterioso
	  (mapcar 'lem:parse-color
		  (remove-duplicates '("#2d3743" "#ff4242" "#74af68" "#dbdb95"
				       "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"
				       ;;above were from ansi-color-names-vector
				       ;;https://github.com/jwiegley/emacs-release/blob/master/etc/themes/misterioso-theme.el
				       "#878787" "#eeeeec" "#415160" "#2d4948"
				       "#212931" "#729fcf" "#23d7d7" "#ffad29"
				       "#e67128")
				     :test 'string=)))
    (progn
      (lem:define-key lem:*global-keymap* "C-/" 'lem:undo)
      (lem:lem "/home/imac/Documents/common-lisp/sucle.lisp")
      (lem:send-event (lambda () (lem-paredit-mode:paredit-mode))))
    (lem-sucle::input-loop)))

(in-package :lem-user)

(define-color-theme "misterioso" ()
  ;;(display-background-mode :dark)
  (foreground "#e1e1e0") ;;
  (background ;;"#3a3a3a" ;;
	      "#2d3743"
	      )
  (cursor :background "#415160"
	  ;;FIXME::what is the correct foreground? ;;not perfect, not same as modeline background,
	  ;;but good enough?
	  :foreground "#212931") ;;
  (region :background "#2d4948" :foreground "#e1e1e0") ;;
  (modeline :background "#212931" :foreground "#eeeeec") ;;
  (modeline-inactive :background "#878787" :foreground "#eeeeec");;
  (minibuffer-prompt-attribute :foreground "#729fcf" :bold-p t) ;;
  (syntax-builtin-attribute :foreground "#23d7d7") ;;
  (syntax-comment-attribute :foreground "#74af68");;
  (syntax-constant-attribute :foreground "#008b8b");;
  (syntax-function-name-attribute :foreground "#00ede1" :bold-p t);;
  (syntax-keyword-attribute :foreground "#ffad29" :bold-p t);;
  (syntax-string-attribute :foreground "#e67128");;
  (syntax-type-attribute :foreground "#34cae2") ;;
  (syntax-variable-attribute :foreground "#dbdb95") ;;
  (syntax-warning-attribute :foreground "#dbdb95" :bold-p t))


(defparameter *packages* nil)
(defun find-lem-package ()
  (remove-if-not (lambda (x)
		   (prefix-p "LEM" 
			     (package-name x)))
		 (list-all-packages)))
;;;FIXME::see cepl.examples/cleanup for similar code
#+nil
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
