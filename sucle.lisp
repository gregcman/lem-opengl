(in-package :lem-sucle)

(defclass sucle (lem:implementation)
  ()
  (:default-initargs
   :native-scroll-support nil
    :redraw-after-modifying-floating-window t))

(setf lem:*implementation* (make-instance 'sucle))

(define-condition exit-editor (lem:editor-condition)
  ((value
    :initarg :value
    :reader exit-editor-value
    :initform nil)))

(struct-to-clos:struct->class
 (defstruct ncurses-view
   scrwin
   modeline-scrwin
   x
   y
   width
   height
   lock))

(defun attribute-to-bits (attribute-or-name)
  (let ((attribute (lem:ensure-attribute attribute-or-name nil))
        (cursorp (eq attribute-or-name 'cursor)))
    (if (null attribute)
        0
        (or (lem::attribute-%internal-value attribute)
            (let* ((foreground (lem:attribute-foreground attribute))
                   (background (lem:attribute-background attribute))
                   (bits (logior (lem.term:get-color-pair foreground background)
                                 0
                                 (if (lem::attribute-bold-p attribute)
                                     ;;charms/ll:a_bold
				     %lem-opengl::a_bold
                                     0)
                                 (if (lem::attribute-underline-p attribute)
                                     ;;charms/ll:a_underline
				     %lem-opengl::a_underline
                                     0)
				 (if (or cursorp (lem::attribute-reverse-p attribute))
				     %lem-opengl::a_reverse
				     0))))
              (setf (lem::attribute-%internal-value attribute) bits)
              bits)))))

;;;mouse stuff copy and pasted from frontends/pdcurses/ncurses-pdcurseswin32
(defvar *dragging-window* ())

(defun mouse-move-to-cursor (window x y)
  (lem:move-point (lem:current-point) (lem::window-view-point window))
  (lem:move-to-next-virtual-line (lem:current-point) y)
  (lem:move-to-virtual-line-column (lem:current-point)
                                   x))
(defun mouse-get-window-rect (window)
  (values (lem:window-x      window)
          (lem:window-y      window)
          (lem:window-width  window)
          (lem:window-height window)))

(defun mouse-event-proc (state x1 y1)
  (lambda ()
    (cond
      ;; button1 down
      ((eq state t)
       (let ((press state))
         (find-if
          (lambda(o)
            (multiple-value-bind (x y w h) (mouse-get-window-rect o)
              (cond
                ;; vertical dragging window
                ((and press (= y1 (- y 1)) (<= x x1 (+ x w -1)))
                 (setf *dragging-window* (list o 'y))
                 t)
                ;; horizontal dragging window
                ((and press (= x1 (- x 1)) (<= y y1 (+ y h -2)))
                 (setf *dragging-window* (list o 'x))
                 t)
                ;; move cursor
                ((and (<= x x1 (+ x w -1)) (<= y y1 (+ y h -2)))
                 (setf (lem:current-window) o)
                 (mouse-move-to-cursor o (- x1 x) (- y1 y))
                 (lem:redraw-display)
                 t)
                (t nil))))
          (lem:window-list))))
      ;; button1 up
      ((null state)
       (let ((o (first *dragging-window*)))
         (when (lem:windowp o)
           (multiple-value-bind (x y w h) (mouse-get-window-rect o)
	     (declare (ignorable x y))
             (setf (lem:current-window) o)
             (cond
               ;; vertical dragging window
               ((eq (second *dragging-window*) 'y)
                (let ((vy (- (- (lem:window-y o) 1) y1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (>= y1       3)
                             (>= (+ h vy) 3))
                    (lem:grow-window vy)
                    (lem:redraw-display))))
               ;; horizontal dragging window
               (t
                (let ((vx (- (- (lem:window-x o) 1) x1)))
                  ;; this check is incomplete if 3 or more divisions exist
                  (when (and (>= x1       5)
                             (>= (+ w vx) 5))
                    (lem:grow-window-horizontally vx)
                    ;; workaround for display update problem (incomplete)
		    #+nil ;;FIXME
                    (%lem-opengl::ncurses-re
		     ;;force-refresh-display ;;charms/ll:*cols*
		     (- ;;charms/ll:*lines*
		      %lem-opengl::*lines*
		      1
		      ))
                    (lem:redraw-display))))
               )))
         (when o
           (setf *dragging-window*
                 (list nil (list x1 y1) *dragging-window*)))))
      )))

(defparameter *saved-session* nil)
(defun input-loop (editor-thread)
  (setf %lem-opengl::*columns* 80
	%lem-opengl::*lines* 25)
  (setf application::*main-subthread-p* nil)
  (application::main
   (lambda ()
     (block out
       (let ((text-sub::*text-data-what-type* :texture-2d))
	 (handler-case
	     (let ((out-token (list "good" "bye")))
	       (catch out-token
		 (loop
		    (per-frame editor-thread out-token))))
	   (exit-editor (c) (return-from out c))))))
   :width (floor (* %lem-opengl::*columns*
		    %lem-opengl::*glyph-width*))
   :height (floor (* %lem-opengl::*lines*
		     %lem-opengl::*glyph-height*))
   :title "lem is an editor for Common Lisp"
   :resizable nil))

(defparameter *scroll-speed* 3)
(defun per-frame (editor-thread out-token)
  (application::on-session-change *saved-session*
    (text-sub::change-color-lookup
     ;;'text-sub::color-fun
     'lem-sucle::color-fun
     #+nil
     (lambda (n)
       (values-list
	(print (mapcar (lambda (x) (utility::floatify x))
		       (nbutlast (aref lem.term::*colors* n))))))
     )
    (application::refresh '%lem-opengl::virtual-window)
    (application::refresh '%lem-opengl::event-queue)
    (window::set-vsync t)
    (lem.term::reset-color-pair))
  (application::getfnc '%lem-opengl::virtual-window)
  (application::getfnc '%lem-opengl::event-queue)
  (application:poll-app)
  (%lem-opengl::per-frame)
  (handler-case
      (progn
	(unless (bt:thread-alive-p editor-thread)
	  (throw out-token nil))
	(resize-event)
	(scroll-event)
	(input-events)	
	(left-click-event)
	#+nil
	(let ((event))
	  
	  (if (eq event :abort)
	      (send-abort-event editor-thread nil)
	      ;;(send-event event)
	      )))
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (lem:send-abort-event editor-thread t))))

(defun left-click-event ()
  (lem:send-event (mouse-event-proc 
		   (window::skey-p
		    (window::mouseval :left)
		    window::*control-state*)
		   (floor window::*mouse-x*
			  %lem-opengl::*glyph-width*)
		   (floor window::*mouse-y*
			  %lem-opengl::*glyph-height*))))

(defun resize-event ()
  (block out
    ;;currently this pops :resize events
    (loop (multiple-value-bind (event exists)
	      (lparallel.queue:try-pop-queue %lem-opengl::*queue*)
	    (if exists
		(lem:send-event event)
		(return-from out))))))

(defun scroll-event ()
  ;;scrolling
  (let ((scroll %lem-opengl::*scroll-difference*))
    (unless (zerop scroll)
      (lem:scroll-up (* *scroll-speed* scroll))
      (lem:redraw-display))))
(defun input-events ()
  (let ((array (window::control-state-jp-or-repeat window::*control-state*)))
    (declare (type window::mouse-keyboard-input-array array))
    (dotimes (code 128)
      (let ((true-p (= 1 (sbit array code))))
	(when true-p
	  (multiple-value-bind (name type) (window::back-value code)
	    ;;(print (list name type))
	    (case type
	      (:key
	       (if (window::character-key-p code)
		   (multiple-value-bind (byte)
		       (character-modifier-bits:character-modifier-bits
			(char-code (char-downcase (code-char code)))
			window::*shift*
			window::*control*
			nil;;window::*alt* ;;FIXME -> makes M-X not M-x
			window::*super*)
		     (let ((key (code-to-key byte)))
		       (lem:send-event
			(lem:make-key
			 :sym (lem:key-sym key)
			 :ctrl (or (lem:key-ctrl key)
				   window::*control*)
			 :shift (or (lem:key-shift key)
					;  window::*shift*
				    )
			 :meta (or (lem:key-meta key)
				   window::*alt*)
			 :super (or (lem:key-super key)
				    window::*super*)))))
		   (let ((key (get-sym-from-glfw3-code name)))
		     (if key
			 (lem:send-event (lem:make-key
					  :sym key
					  :meta window::*alt*
					  :super window::*super*
					  :shift window::*shift*
					  :ctrl window::*control*))
			 (format *error-output*
				 "~s key unimplemented" name))))))))))))

(lem:add-hook
 lem:*before-init-hook*
 (lambda ()
   (lem:load-theme "emacs-dark")))

(defmethod lem-if:invoke ((implementation sucle) function)
  (let ((result nil)
        (input-thread (bt:current-thread)))
    (lem.term:term-init)
    (let ((editor-thread
	   (funcall function
		    nil
		    (lambda (report)
		      (bt:interrupt-thread
		       input-thread
		       (lambda ()
			 (print report)
			 (error 'exit-editor :value report)))))))
      (setf result (input-loop editor-thread)))
    (when (and (typep result 'exit-editor)
               (exit-editor-value result))
      (format t "~&~A~%" (exit-editor-value result)))))

(defmethod lem-if:display-background-mode ((implementation sucle))
  (lem.term:background-mode))

(defmethod lem-if:update-foreground ((implementation sucle) color-name)
  (lem.term:term-set-foreground color-name))

(defmethod lem-if:update-background ((implementation sucle) color-name)
  (lem.term:term-set-background color-name))

(defmethod lem-if:display-width ((implementation sucle))
  (max 5
       %lem-opengl::*columns*
       ;;charms/ll:*cols*
       ))

(defmethod lem-if:display-height ((implementation sucle))
  (max 3
       %lem-opengl::*lines*
       ;;charms/ll:*lines*
       ))

(defmethod lem-if:make-view
    ((implementation sucle) window x y width height use-modeline)
  (flet ((newwin (nlines ncols begin-y begin-x main-screen)
           (declare (ignore main-screen))
           (let ((win
		  (;;charms/ll:newwin
		   %lem-opengl::ncurses-newwin
		   nlines ncols begin-y begin-x)))
	     (when use-modeline (;;charms/ll:keypad
				 %lem-opengl::ncurses-keypad
				 win 1))
             ;; (when main-screen
             ;;   (charms/ll:idlok win 1)
             ;;   (charms/ll:scrollok win 1))
             win)))
    (make-ncurses-view
     :scrwin (newwin height width y x nil)
     :modeline-scrwin (when use-modeline (newwin 1 width (+ y height) x nil))
     :x x
     :y y
     :width width
     :height height
     :lock (bt:make-recursive-lock "window-lock"))))

(defmacro with-view-lock (view &body body)
  (utility::with-gensyms (lock)
    `(let ((,lock (ncurses-view-lock ,view)))
       (bt:with-recursive-lock-held (,lock)
	 ,@body))))


(defmethod lem-if:delete-view ((implementation sucle) view)
  (with-view-lock view
    (;;charms/ll:delwin
     %lem-opengl::ncurses-delwin
     (ncurses-view-scrwin view))
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:delwin
       %lem-opengl::ncurses-delwin
       (ncurses-view-modeline-scrwin view)))))

(defmethod lem-if:clear ((implementation sucle) view)
  ;;;https://linux.die.net/man/3/clearok
  (with-view-lock view
    (;;charms/ll:clearok
     %lem-opengl::ncurses-clearok
     (ncurses-view-scrwin view) 1)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:clearok
       %lem-opengl::ncurses-clearok
       (ncurses-view-modeline-scrwin view) 1))))

(defmethod lem-if:set-view-size ((implementation sucle) view width height)
  (with-view-lock view
    (setf (ncurses-view-width view) width)
    (setf (ncurses-view-height view) height)
    (;;charms/ll:wresize
     %lem-opengl::ncurses-wresize
     (ncurses-view-scrwin view) height width)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:mvwin
       %lem-opengl::ncurses-mvwin
       (ncurses-view-modeline-scrwin view)
       (+ (ncurses-view-y view) height)
       (ncurses-view-x view))
      (;;charms/ll:wresize
       %lem-opengl::ncurses-wresize
       (ncurses-view-modeline-scrwin view)
       (lem:minibuffer-window-height)
       width))))

(defmethod lem-if:set-view-pos ((implementation sucle) view x y)
  (with-view-lock view
    (setf (ncurses-view-x view) x)
    (setf (ncurses-view-y view) y)
    (;;charms/ll:mvwin
     %lem-opengl::ncurses-mvwin
     (ncurses-view-scrwin view) y x)
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:mvwin
       %lem-opengl::ncurses-mvwin
       (ncurses-view-modeline-scrwin view)
       (+ y (ncurses-view-height view))
       x))))

(defmethod lem-if:print ((implementation sucle) view x y string attribute)
  (with-view-lock view
    (let ((attr (attribute-to-bits attribute)))
      (;;charms/ll:wattron
       %lem-opengl::ncurses-wattron
       (ncurses-view-scrwin view) attr)
      ;;(charms/ll:scrollok (ncurses-view-scrwin view) 0)
      (;;charms/ll:mvwaddstr
       %lem-opengl::ncurses-mvwaddstr
       (ncurses-view-scrwin view) y x string)
      ;;(charms/ll:scrollok (ncurses-view-scrwin view) 1)
      (;;charms/ll:wattroff
       %lem-opengl::ncurses-wattroff
       (ncurses-view-scrwin view) attr))))

(defmethod lem-if:print-modeline ((implementation sucle) view x y string attribute)
  (with-view-lock view
    (let ((attr (attribute-to-bits attribute)))
      (;;charms/ll:wattron
       %lem-opengl::ncurses-wattron
       (ncurses-view-modeline-scrwin view) attr)
      (;;charms/ll:mvwaddstr
       %lem-opengl::ncurses-mvwaddstr
       (ncurses-view-modeline-scrwin view) y x string)
      (;;charms/ll:wattroff
       %lem-opengl::ncurses-wattroff
       (ncurses-view-modeline-scrwin view) attr))))

(defmethod lem-if:clear-eol ((implementation sucle) view x y)
  (with-view-lock view
    (;;charms/ll:wmove
     %lem-opengl::ncurses-wmove
     (ncurses-view-scrwin view) y x)
    (;;charms/ll:wclrtoeol
     %lem-opengl::ncurses-wclrtoeol
     (ncurses-view-scrwin view))))

(defmethod lem-if:clear-eob ((implementation sucle) view x y)
  (with-view-lock view
    (;;charms/ll:wmove
     %lem-opengl::ncurses-wmove
     (ncurses-view-scrwin view) y x)
    (;;charms/ll:wclrtobot
     %lem-opengl::ncurses-wclrtobot
     (ncurses-view-scrwin view))))

;;(defparameter *no* *standard-output*)

(defmethod lem-if:redraw-view-after ((implementation sucle) view focus-window-p)
  (with-view-lock view
    #+nil ;;;FIXME
    (let ((attr (attribute-to-bits 'modeline)))
      (;;charms/ll:attron
       %lem-opengl::ncurses-attron
       attr)
      #+nil ;;FIXME:: disabling 
      (when (and (ncurses-view-modeline-scrwin view)
		 (< 0 (ncurses-view-x view)))
	(;;charms/ll:move
	 %lem-opengl::ncurses-move
	 (ncurses-view-y view)
	 (1- (ncurses-view-x view)))
	(;;charms/ll:vline
	 %lem-opengl::ncurses-vline
	 (char-code #\space)
	 (1+ (ncurses-view-height view))))
      (;;charms/ll:attroff
       %lem-opengl::ncurses-attroff
       attr)
      (;;charms/ll:wnoutrefresh
       %lem-opengl::ncurses-wnoutrefresh
       ;;charms/ll:*stdscr*
       %lem-opengl::*std-scr*))
    (when (ncurses-view-modeline-scrwin view)
      (;;charms/ll:wnoutrefresh
       %lem-opengl::ncurses-wnoutrefresh
       (ncurses-view-modeline-scrwin view))
      ;;   (%lem-opengl::print-virtual-window %lem-opengl::*virtual-window* *no*)
      )
    
    (;;charms/ll:wnoutrefresh
     %lem-opengl::ncurses-wnoutrefresh
     (ncurses-view-scrwin view)))

  ;;  (%lem-opengl::print-virtual-window %lem-opengl::*virtual-window* *no*)
  )

(defmethod lem-if:update-display ((implementation sucle))
  (let ((view (lem:window-view (lem:current-window))))
    (with-view-lock view
      (let ((scrwin (ncurses-view-scrwin view)))
	(if (lem::covered-with-floating-window-p
	     (lem:current-window)
	     lem::*cursor-x* lem::*cursor-y*)
	    (;;charms/ll:curs-set
	     %lem-opengl::ncurses-curs-set
	     0)
	    (progn
	      (;;charms/ll:curs-set
	       %lem-opengl::ncurses-curs-set
	       1)
	      (;;charms/ll:wmove
	       %lem-opengl::ncurses-wmove
	       scrwin lem::*cursor-y* lem::*cursor-x*)))
	;;FIXME
	(;;charms/ll:wnoutrefresh
	 %lem-opengl::ncurses-wnoutrefresh
	 scrwin)
	(;;charms/ll:doupdate
	 %lem-opengl::ncurses-doupdate)))))

(defmethod lem-if:scroll ((implementation sucle) view n)
  (with-view-lock view
    (;;charms/ll:wscrl
     %lem-opengl::ncurses-wscrl
     (ncurses-view-scrwin view)
     n)))

(defmethod lem-if:clipboard-paste ((implementation sucle))
  (trivial-clipboard:text))

(defmethod lem-if:clipboard-copy ((implementation sucle) text)
  (trivial-clipboard:text text))

(pushnew :lem-sucle *features*)

;;#+nil
(defun c6? (x)
  (let ((acc nil))
    (loop
       (push (mod x 6) acc)
       (setf x (floor x 6))
       (when (zerop x)
	 (return)))
    acc))

(defun color-fun (color)
  (labels ((bcolor (r g b)
	     (values (/ (utility::floatify r) 255.0)
		     (/ (utility::floatify g) 255.0)
		     (/ (utility::floatify b) 255.0)))
	   (c (r g b)
	     (bcolor r g b))
	   (c6 (x)
	     (destructuring-bind (r g b) (last (append (list 0 0 0)
						       (c6? x))
					       3)
	       (bcolor (* 51 r)
		       (* 51 g)
		       (* 51 b))))
	   (g (x)
	     (let* ((magic (load-time-value (/ 255.0 23.0)))
		    (val (* x magic)))
	       (c val val val))))
    (case color
      (0 (c 0 0 0))
      (1 (c 205 0 0))
      (2 (c 0 205 0))
      (3 (c 205 205 0))
      (4 (c 0 0 238))
      (5 (c 205 0 205))
      (6 (c 0 205 205))
      (7 (c 229 229 229))
      (8 (c 127 127 127))
      (9 (c 255 0 0))
      (10 (c 0 255 0))
      (11 (c 255 255 0))
      (12 (c 92 92 255))
      (13 (c 255 0 255))
      (14 (c 0 255 255))
      (15 (c 255 255 255))
      (t (if (< color (+ 16 (* 6 6 6)))
	     (c6 (- color 16))
	     (g (- color (+ 16 (* 6 6 6)))))))))

(defun start-lem ()
  (let ((lem::*in-the-editor* nil))
    (lem:main '("/home/imac/Documents/common-lisp/sucle.lisp"))))
