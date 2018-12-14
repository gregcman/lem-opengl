(in-package :lem-sucle)

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
     'lem.term::color-fun
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
