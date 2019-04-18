(in-package :lem-sucle)

(defparameter *glyph-height* 16.0)
(defparameter *glyph-width* 8.0)

(defparameter *queue* nil)
(application::deflazy event-queue ()
  (setf *queue* (lparallel.queue:make-queue)))
(application::deflazy virtual-window ((w application::w) (h application::h) (event-queue event-queue))
  (lparallel.queue:push-queue :resize event-queue)
  (setf ncurses-clone::*columns* (floor w *glyph-width*)
	ncurses-clone::*lines* (floor h *glyph-height*))
  ;;(ncurses-clone::reset-standard-screen)
  (ncurses-clone::with-virtual-window-lock
    (ncurses-clone::ncurses-wresize ncurses-clone::*std-scr*
				    ncurses-clone::*lines*
				    ncurses-clone::*columns*)
    #+nil
    (setf ncurses-clone::*virtual-window*
	  (ncurses-clone::make-virtual-window))))

(defparameter *saved-session* nil)
(defun input-loop (&optional (editor-thread lem-sucle::*editor-thread*))
  (setf ncurses-clone::*columns* 80
	ncurses-clone::*lines* 25)
  (setf application::*main-subthread-p* nil)
  (set-glyph-dimensions 8 16)
  (application::main
   (lambda ()
     (block out
       (let ((text-sub::*text-data-what-type* :texture-2d))
	 (handler-case
	     (let ((out-token (list "good" "bye")))
	       (catch out-token
		 (loop
		    (livesupport:update-repl-link)
		    (livesupport:continuable
		      (per-frame editor-thread out-token)))))
	   (exit-editor (c) (return-from out c))))))
   :width (floor (* ncurses-clone::*columns*
		    *glyph-width*))
   :height (floor (* ncurses-clone::*lines*
		     *glyph-height*))
   :title "lem is an editor for Common Lisp"
   :resizable t))

(defun set-glyph-dimensions (w h)
  ;;Set the pixel dimensions of a 1 wide by 1 high character
  (setf *glyph-width* w)
  (setf text-sub::*block-width* w)
  (setf *glyph-height* h)
  (setf text-sub::*block-height* h)
  (progn
    ;;FIXME::Better way to organize this? as of now manually determining that
    ;;these two depend on the *block-height* and *block-width* variables
    (application::refresh 'text-sub::render-normal-text-indirection)
    (application::refresh 'virtual-window)))

(defparameter *redraw-display-p* nil)
(defun redraw-display ()
  (setf *redraw-display-p* t))
(defparameter *last-scroll* 0)
(defparameter *scroll-difference* 0)
(defparameter *scroll-speed* 5)
(defparameter *run-sucle* nil)
(defun per-frame (editor-thread out-token)
  (declare (ignorable editor-thread))
  (application::on-session-change *saved-session*
    (text-sub::change-color-lookup
     ;;'text-sub::color-fun
     'lem.term::color-fun
     #+nil
     (lambda (n)
       (values-list
	(print (mapcar (lambda (x) (/ (utility::floatify x) 255.0))
		       (nbutlast (aref lem.term::*colors* n))))))
     )
    (application::refresh 'virtual-window)
    (application::refresh 'event-queue)
    (window::set-vsync t)
    ;;(lem.term::reset-color-pair)
    )
  (application::getfnc 'virtual-window)
  (application::getfnc 'event-queue)
  (application:poll-app)
  (when *run-sucle*
    (unwind-protect
	 (application::with-quit-token ()
	   (funcall sucle::*sucle-app-function*))
      (setf *run-sucle* nil)
      (window:get-mouse-out)))
  (let ((newscroll (floor window::*scroll-y*)))
    (setf *scroll-difference* (- newscroll *last-scroll*))
    (setf *last-scroll* newscroll))
  
  (handler-case
      (progn
	(when window::*status*
	  ;;(bt:thread-alive-p editor-thread)
	  (throw out-token nil))
	(resize-event)
	(scroll-event)
	(input-events)
	;;#+nil
	(calculate-cursor-coordinate)
	(left-click-event)
	#+nil
	(let ((event))
	  
	  (if (eq event :abort)
	      (send-abort-event editor-thread nil)
	      ;;(send-event event)
	      )))
    #+nil
    #+sbcl
    (sb-sys:interactive-interrupt (c)
      (declare (ignore c))
      (lem:send-abort-event editor-thread t)))

  ;;Flush the changes made to the ncurses-clone display
  (when *redraw-display-p*
    (setf *redraw-display-p* nil)
    (lem:redraw-display))
  
  ;;Rendering. Comes after input handling because things could have changed
  (progn
    (glhelp:set-render-area 0 0 window:*width* window:*height*)
    ;;(gl:clear-color 0.0 0.0 0.0 0.0)
    ;;(gl:clear :color-buffer-bit)
    (gl:polygon-mode :front-and-back :fill)
    (gl:disable :cull-face)
    (gl:disable :depth-test)
    (gl:disable :blend)

    (render-stuff)))
(defparameter *output* *standard-output*)
(defparameter *mouse-last-position* nil)
(defparameter *point-at-last* nil)
(defparameter *mouse-mode* nil)

(defparameter *last-clicked-at* nil) ;;to detect double and triple clicks etc...
(defparameter *point-clicked-at* nil) ;;point representing the starting location in the buffer
(defparameter *clicked-at-times* 0)
(defparameter *click-selection-count* 0)
;;the number of times clicks consecutively at a position,
;;starting with 1
(defun reset-mouse-mode ()
  (setf *mouse-mode* nil))
(defparameter *marking-window* nil)

(defun same-buffer-points (a b)
  (eq 
   (lem:point-buffer a)
   (lem:point-buffer b)))

(defparameter *grid-mouse-x* nil)
(defparameter *grid-mouse-y* nil)
(defun calculate-cursor-coordinate ()
  ;;For some reason, the coordinate of the mouse is off by 1,1?
  (setf *grid-mouse-x*
	(floor (- window::*mouse-x* 1)
	       *glyph-width*))
  (setf *grid-mouse-y*
	(floor (- 
		window::*mouse-y*
		;;There's a little space between the edge of the window and the area lem uses,
		;;since the window coordinates are not necessary multiples of the glyph size
		;;This causes the y position of the cursor to become messed up, if not accounted for
		(mod window::*height*
		     *glyph-height*)
		1)
	       *glyph-height*)))
(defun left-click-event ()
  ;;#+nil
  (let ((just-pressed (window::skey-j-p
		       (window::mouseval :left)
		       window::*control-state*))
	(just-released (window::skey-j-r
			(window::mouseval :left)
			window::*control-state*))
	(pressing 
	 (window::skey-p
	  (window::mouseval :left)
	  window::*control-state*)))
    (let* ((coord (list *grid-mouse-x* *grid-mouse-y*))
	   (coord-change
	     (if (not (equal coord
			     *mouse-last-position*))
		 (progn (setf *mouse-last-position* coord)
			t)
		 nil))
	   ;;'different' is used to track whether changes happened, and whether or not
	   ;;things should be updated
	   (different (or
		       ;;pressing and having a coord change = dragging
		       (and pressing
			    coord-change)
		       just-released
		       just-pressed)))
      ;;FIXME::better logic? comments?
      (when (or different)
	(when pressing
	  (let ((window (detect-mouse-window-intersection)))
	    (when window
	      (when (centered-between window)
		(move-window-cursor-to-mouse window)
		;;FIXME::Clicking on the minibuffer is causing errors.
		;;Are the wrong points being used?
		(setf (lem:current-window) window)

		(when (eq *mouse-mode* :marking)
		  (when (not (eq *marking-window*
				 window))
		    (setf *marking-window* window)
		    (reset-mouse-mode)))
		
		(redraw-display)) ;;FIXME::this occurs below as well.
	      ))))
      ;;switch to window that the mouse is hovering over, and find that file
      (when window::*dropped-files*
	(let ((window
	       (detect-mouse-window-intersection)))
	  (when window
	    (setf (lem:current-window) window)))
	(unless
	    ;;Do not drop a file into the minibuffer
	    (eq lem::*minibuf-window*
		(lem:current-window))
	  (dolist (file window::*dropped-files*)
	    (lem:find-file file))
	  (redraw-display));;FIXME::this occers below as well. set a flag instead?
	)
      (when just-released
	(case *mouse-mode*
	  (:marking (reset-mouse-mode))))
      ;;TODO::handle selections across multiple windows?
      (when just-pressed
	;;(print "cancelling")
	(if (equal *last-clicked-at* coord)
	    (incf *clicked-at-times*)
	    (progn
	      (setf *last-clicked-at* coord)
	      (setf *clicked-at-times* 1)))
	(flet ((cancel-click-selection ()
		 (lem:buffer-mark-cancel (lem:current-buffer))
		 (setf *click-selection-count* 0)))
	  (cond
	    ((= 1 *clicked-at-times*)
	     (cancel-click-selection)
	     (setf *point-clicked-at*
		   (lem:copy-point (lem:current-point)
				   :temporary)))
	    ((< 1 *clicked-at-times*)
	     ;;(print *clicked-at-times*)
	     
	     ;;(lem:save-excursion)
	     (let ((successp t))
	       (incf *click-selection-count*)
	       (let (inside
		     (on-last-paren nil))
		 (lem:with-point ((start *point-clicked-at*)
				  (end *point-clicked-at*))
		   (handler-case (progn				 
				   (lem:move-point (lem:current-point) *point-clicked-at*)
				   (lem:forward-sexp) ;;fails if on a closing paren
				   (lem:move-point end (lem:current-point))
				   (lem:backward-sexp) ;;fails at first char in list
				   (lem:move-point start (lem:current-point))
				   (setf inside
					 (and (lem:point<= start *point-clicked-at*)
					      (lem:point< *point-clicked-at* end))))
		     (lem:editor-error (c)
		       (declare (ignorable c))
		       (setf on-last-paren t)))
		   (let ((iteration-count *click-selection-count*))
		     ;;(print (list inside on-last-paren))
		     (when inside
		       (decf iteration-count))
		     (dotimes (i iteration-count)
		       (handler-case (progn
				       ;;(lem:save-excursion
				       (lem:backward-up-list)
				       )
			 (lem:editor-error (c)
			   (declare (ignorable c))
			   ;;turn this on to select the whole buffer 
			   ;;(lem::mark-set-whole-buffer)
			   (setf successp nil)))))
		   (if successp
		       (progn
			 ;;(lem:move-point (lem:current-point) start)
			 (lem:set-current-mark (lem:copy-point (lem:current-point)
							       :temporary))
			 (lem:mark-sexp))
		       (progn (cancel-click-selection)
			      (lem:move-point (lem:current-point)
					      *point-clicked-at*)))))))))
	(case *mouse-mode*
	  (:marking (reset-mouse-mode))))
      (let ((last-point *point-at-last*)
	    (point-coord-change		
	     (let ((point (lem:current-point)))
	       (if (or (not *point-at-last*)
		       (not
			(and
			 ;;make sure they are in the same buffer
			 (same-buffer-points point *point-at-last*)
			 ;;then check whether they are equal
			 (lem:point= point
				     *point-at-last*))))
		   (progn
		     ;;(print (list *point-at-last* point))
		     (when *point-at-last*
		       (lem:delete-point *point-at-last*))
		     (setf *point-at-last*
			   (lem:copy-point point
					   :temporary)
			   ;;FIXME:: do we want :temporary points?
			   ;;does it create garbage?
			   )
		     #+nil
		     (print (lem-base::buffer-points
			     (lem:point-buffer point)))
		     t)
		   (progn
		     nil)))))
	(when (and
	       pressing
	       (not (eq *mouse-mode* :marking))
	       (not just-pressed) ;;if it was just pressed, there's going to be a point-coord jump
	       point-coord-change ;;selecting a single char should not start marking
	       )
	  ;;beginning to mark
	  ;;(print 3434)
	  (let ((current-point (lem:current-point)))
	    (if (and
		 (not (null last-point))
		 (same-buffer-points current-point last-point))		
		(lem:set-current-mark last-point)
		(progn
		  ;;FIXME? when does this happen? when the last point is null or
		  ;;exists in a different buffer? allow buffer-dependent selection?
		  (lem:set-current-mark current-point))))
	  ;;(lem-base:region-end)
	  ;;(redraw-display)
	  (setf *mouse-mode* :marking)))
      (when different
	(redraw-display)))))

;;;mouse stuff copy and pasted from frontends/pdcurses/ncurses-pdcurseswin32
(defvar *dragging-window* ())

(defun move-window-cursor-to-mouse (window &optional (x1 *grid-mouse-x*) (y1 *grid-mouse-y*))
  (let ((x (lem:window-x window))
	(y (lem:window-y window)))
    (mouse-move-to-cursor window (- x1 x) (- y1 y))))

(defun mouse-move-to-cursor (window x y)
  (let ((point (lem:current-point))
	(view-point (lem::window-view-point window)))
    ;;view-point is in the very upper right
    (when (same-buffer-points point view-point)
      (lem:move-point point view-point)
      (lem:move-to-next-virtual-line point y)
      (lem:move-to-virtual-line-column point x))))
#+nil
(defun mouse-get-window-rect (window)
  (values (lem:window-x      window)
          (lem:window-y      window)
          (lem:window-width  window)
          (lem:window-height window)))

(defun horizontally-between (window &optional (x1 *grid-mouse-x*))
  (let ((x (lem:window-x window))
	(w (lem:window-width window)))
    (and (<= x x1) (< x1 (+ x w)))))
(defun vertically-between (window &optional (y1 *grid-mouse-y*))
  (let ((y (lem:window-y window))
	(h (lem:window-height window)))
    (and (<= y y1)
	 (< y1
	    (+ y h
	       (if (lem::window-use-modeline-p window)
		   -1
		   0))))))
(defun centered-between (window &optional (x1 *grid-mouse-x*) (y1 *grid-mouse-y*))
  (and (horizontally-between window x1)
       (vertically-between window y1)))

(defun detect-mouse-window-intersection
    (&optional (x1 *grid-mouse-x*) (y1 *grid-mouse-y*)
       ;; &optional (press nil)
	    )
  ;;find the window which the coordinates x1 and y1 intersect at and return the window
  ;;and intersection type
  ;;returns (values window[if found window, otherwise nil] intersection-type)
  ;;intersection is one of :vertical, :horizontal, :center, or nil
  (let ((windows (lem:window-list)))
    #+nil;;FIXME::what does this variable do in lem?
    (when lem::*minibuffer-calls-window*
      (push lem::*minibuffer-calls-window* windows))
    (when lem::*minibuf-window*
      (push lem::*minibuf-window* windows))
    (block return 
      (dolist (window windows) 
	(let ((x (lem:window-x window))
	      (y (lem:window-y window))) 
	  #+nil
	  (when (eq window lem::*minibuf-window*)
	    ;;(print (list x y w h x1 y1))
	    )
	  
	  (cond
	    ;; vertical dragging window
	    ((and (= y1 (- y 1))
		  (horizontally-between window x1))
	     ;;(setf *dragging-window* (list window 'y))
	     (return-from return (values window :vertical)))
	    ;; horizontal dragging window	    
	    ((and (= x1 (- x 1))
		  (vertically-between window y1))
	     ;;(setf *dragging-window* (list window 'x))
	     (return-from return (values window :horizontal)))
	    ((centered-between window x1 y1)
	     (return-from return (values window :center)))
	    (t))))
      (values nil nil))))

#+nil
(defun mouse-event-proc (state x1 y1)
  (lambda ()
    (cond
      ;; button1 down
      ((eq state t)
       (let ((press state))
         (switch-to-window-mouse x1 y1 press)))
      ;; button1 up
      #+nil
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
                    (ncurses-clone::ncurses-re
		     ;;force-refresh-display ;;charms/ll:*cols*
		     (- ;;charms/ll:*lines*
		      ncurses-clone::*lines*
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
	      (lparallel.queue:try-pop-queue *queue*)
	    (if exists
		(lem:send-event event)
		(return-from out))))))

(defun scroll-event ()
  ;;scrolling
  (let ((scroll *scroll-difference*))
    (unless (zerop scroll)
      (lem:scroll-up (* *scroll-speed* scroll))
      (redraw-display)
      )))
(defun input-events ()
  ;;(print (list window::*control* window::*alt* window::*super*))
  ;;unicode input
  (dolist (press window::*char-keys*)
    (destructuring-bind (byte mods) press
      (let ((key (code-to-key byte)))
	(unless
	    ;;FIXME::better logic to handle this? ;;This is because space gets sent twice,
	    ;;once as a unicode char and once as a control key. The control key is for
	    ;;exampe C-Space
	    (member byte (load-time-value (list (char-code #\Space))))
	  (lem:send-event
	   (lem:make-key
	    :sym (lem:key-sym key)
	    :ctrl (or (lem:key-ctrl key)
		      (logtest window::+control+ mods)
		      (window:skey-p (window::keyval :escape))
		      ;;FIXME:: escape used as substitute for control, specifically windows.
		      ;;see below for same info.
		      )
	    :shift (or (lem:key-shift key)
		       ;;window::*shift* ;;FIXME::why is this here?
		       )
	    :meta (or (lem:key-meta key)
		      (logtest window::+alt+ mods))
	    :super (or (lem:key-super key)
		       (logtest window::+super+ mods))))))))
  ;;control key input, such as Tab, delete, enter
  (let ((array (window::control-state-jp-or-repeat window::*control-state*)))
    (declare (type window::mouse-keyboard-input-array array))
    (dotimes (code 128)
      (let ((true-p (= 1 (sbit array code))))
	(when true-p
	  (multiple-value-bind (name type) (window::back-value code)
	    ;;(print (list name type))
	    (case type
	      (:key ;;FIXME::add mouse support?
	       (cond ((and (window::character-key-p code)
			   (not (member name '(:space)));;;FIXME::better logic to handle this?
			   ))
		     (t
		      (if (member name
				  '(:left-shift :left-control :left-super :left-alt
				    :right-shift :right-control :right-super :right-alt
				    :escape ;;FIXME::escape used as substitute for control,
				    ;;specifically for windows
				    ))
			  ;;FIXME::more efficient test?
			  nil ;;;ignore the modifier keys for shift, super, alt, control
			  (let ((key (get-sym-from-glfw3-code name)))
			    (if key
				(lem:send-event (lem:make-key
						 :sym key
						 :meta window::*alt*
						 :super window::*super*
						 :shift window::*shift*
						 :ctrl window::*control*))
				(format *error-output*
					"~s key unimplemented" name))))))))))))))

(defun render-stuff ()
  #+nil
  (;;text-sub::with-data-shader (uniform rebase)
   ;; (gl:clear :color-buffer-bit)
 ;;   (gl:disable :depth-test)
    #+nil
    (rebase -128.0 -128.0))
  #+nil
  (gl:point-size 1.0)

  ;;;;what? this is to replace (gl:with-primitives :points ...body)
  ;;;; to find bug where resizing the lem window over and over causes crash
  #+nil
  (unwind-protect (progn
		    (gl:begin :points)
		    (opengl-immediate::mesh-vertex-color))
    (gl:end))
  (when ncurses-clone::*update-p*
    (setf ncurses-clone::*update-p* nil)
    ;;Set the title of the window to the name of the current buffer
    (window:set-caption (lem-base:buffer-name (lem:current-buffer)))
    ;;;Copy the virtual screen to a c-array,
    ;;;then send the c-array to an opengl texture
    (let* ((c-array-lines
	    (min text-sub::*text-data-height* ;do not send data larger than text data
		 (+ 1 ncurses-clone::*lines*)))              ;width or height
	   (c-array-columns
	    (min text-sub::*text-data-width*
		 (+ 1 ncurses-clone::*columns*)))
	   (c-array-len (* 4
			   c-array-columns
			   c-array-lines)))
      (cffi:with-foreign-object
       (arr :uint8 c-array-len)
       (flet ((color (r g b a x y)
		(let ((base (* 4 (+ x (* y c-array-columns)))))
		  (setf (cffi:mem-ref arr :uint8 (+ 0 base)) r
			(cffi:mem-ref arr :uint8 (+ 1 base)) g
			(cffi:mem-ref arr :uint8 (+ 2 base)) b
			(cffi:mem-ref arr :uint8 (+ 3 base)) a))))
	 (progn
	   (let ((foox (- c-array-columns 1))
		 (bary (- c-array-lines 1)))
	     (flet ((blacken (x y)
		      (color 0 0 0 0 x y)))
	       (blacken foox bary)
	       (dotimes (i bary)
		 (blacken foox i))
	       (dotimes (i foox)
		 (blacken i bary)))))
	 
	 (let ((len ncurses-clone::*lines*))
	   (dotimes (i len)
	     (let ((array (aref (ncurses-clone::win-data ncurses-clone::*std-scr*) (- len i 1)))
		   (index 0))
	       (block out
		 (do ()
		     ((>= index ncurses-clone::*columns*))
		   (let* ((glyph (aref array index)))

		     ;;This occurs if the widechar is overwritten, but the placeholders still remain.
		     ;;otherwise it would be skipped.
		     (when (eq glyph ncurses-clone::*widechar-placeholder*)
		       (setf glyph ncurses-clone::*clear-glyph*))
		     
		     (let* ((glyph-character (ncurses-clone::glyph-value glyph))
			    (width (ncurses-clone::char-width-at glyph-character index)))

		       ;;FIXME::for some reason, when resizing really quickly,
		       ;;this can get screwed up, so bail out
		       (when (<= (+ 1 ncurses-clone::*columns*)
				 (+ width index))
			 (return-from out))
		       (let* ((attributes (ncurses-clone::glyph-attributes glyph))
			      (pair (ncurses-clone::ncurses-color-pair (mod attributes 256))))
			 (let ((realfg
				(let ((fg (car pair)))
				  (if (or
				       (not pair)
				       (= -1 fg))
				      ncurses-clone::*fg-default* ;;FIXME :cache?
				      fg)))
			       (realbg
				(let ((bg (cdr pair)))
				  (if (or
				       (not pair)
				       (= -1 bg))
				      ncurses-clone::*bg-default* ;;FIXME :cache?
				      bg))))
			   (when (logtest ncurses-clone::A_reverse attributes)
			     (rotatef realfg realbg))
			   (dotimes (offset width)
			     (block abort-writing
			       (color 
				;;FIXME::this is temporary, to chop off extra unicode bits
				(let ((code (char-code glyph-character)))
				  (if (ncurses-clone::less-than-256-p code)
				      code
				      ;;Draw nice-looking placeholders for unimplemented characters.
				      ;;1 wide -> #
				      ;;n wide -> {@...}
				      (case width
					(1 (load-time-value (char-code #\#)))
					(otherwise
					 (cond 
					   ((= offset 0)
					    (load-time-value (char-code #\{)))
					   (t
					    (let ((old-thing (aref array (+ index offset))))
					      (if (eq ncurses-clone::*widechar-placeholder*
						      old-thing)
						  (if (= offset (- width 1))
						      (+ (load-time-value (char-code #\})))
						      (load-time-value (char-code #\@)))
						  ;;if its not a widechar-placeholder, the placeholder
						  ;;was overwritten, so don't draw anything.
						  (return-from abort-writing)))))))))
				realfg
				realbg
				(text-sub::char-attribute
				 (logtest ncurses-clone::A_bold attributes)
				 (logtest ncurses-clone::A_Underline attributes)
				 t)
				(+ offset index)
				i)))))
		       (incf index width)))))))))
       ;;;;write the data out to the texture
       (let ((texture (text-sub::get-text-texture)))
	 (gl:bind-texture :texture-2d texture)
	 (gl:tex-sub-image-2d :texture-2d 0 0 0
			      c-array-columns
			      c-array-lines
			      :rgba :unsigned-byte arr)))))
  (text-sub::with-text-shader (uniform)
    (gl:uniform-matrix-4fv
     (uniform :pmv)
     (load-time-value (nsb-cga:identity-matrix))
     nil)   
    (glhelp::bind-default-framebuffer)
    (glhelp:set-render-area 0
			    0
			    window::*width*
			    window::*height*)
    #+nil
    (progn
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))

    (text-sub::draw-fullscreen-quad)))

#+nil
(let ((width 
       (lem-base:char-width char 0)))

  ;;FIXME::have option to turn this off
  (dotimes (i width)
    (add-char x y
	      ;; 0
	      
	      
	      win) ;;FIXME::magically adding a null character
    (advance)))
	   ;;(error "what char? ~s" (char-code char))
