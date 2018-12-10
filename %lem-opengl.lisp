(defpackage #:%lem-opengl
  (:use #:cl #:utility #:application #:opengl-immediate
	#:sprite-chain #:point #:rectangle)
  (:export #:start))
(in-package :%lem-opengl)

(defparameter *ticks* 0)
(defparameter *saved-session* nil)
(defun per-frame ()
  (on-session-change *saved-session*
    (init))
  (incf *ticks*)
  (app))

(defparameter *glyph-height* 16.0)
(defparameter *glyph-width* 8.0)

(defparameter *columns* 80)
(defparameter *lines* 25)

;;(defparameter *app* nil)
#+nil
(defun start ()
  (application:main
   (lambda ()
     (loop
	(application:poll-app)
					;(if *app*)
	;;(testbed::per-frame)
	(progn
	  ;;#+nil
	  (per-frame)
	  #+nil
	  (when (window:skey-j-p (window::keyval #\e))
	    (window::toggle-mouse-capture)))
	#+nil
	(when (window:skey-j-p (window::keyval #\h))
	  (toggle *app*))))
   :width (floor (* *columns* *glyph-width*))
   :height (floor (* *lines* *glyph-height*))
   :title ""))

#+nil
(struct-to-clos:struct->class
 (defstruct glyph
   value
   attributes))

(deftype glyph () `(unsigned-byte 10))
(defun glyph-value (glyph)
  (declare (type fixnum glyph))
  (declare (optimize (speed 3)
		     (safety 0)))
  (code-char (logand glyph (utility::etouq (1- (expt 2 8))))))
(defun glyph-attributes (glyph)
  (declare (type fixnum glyph))
  (declare (optimize (speed 3)
		     (safety 0)))
  (ash (logand glyph (utility::etouq (lognot (1- (expt 2 8))))) -8))

(defun gen-glyph (value attributes)
  (declare (optimize (speed 3)
		     (safety 0)))
  #+nil
  (make-glyph :value value
	      :attributes attributes)
  (logior (char-code value)
	  (the fixnum (ash (the fixnum attributes) 8))))

#+nil
(progn
  (defun print-glyph (stream glyph)
    (write-char (glyph-value glyph) stream))
  (set-pprint-dispatch 'glyph 'print-glyph))

(defparameter *clear-glyph* (gen-glyph #\Space 0))


(defun make-virtual-window ()
  (let ((array (make-array *lines*)))
    (dotimes (i (length array))
      (setf (aref array i)
	    (make-array *columns*
			:initial-element *clear-glyph*)))
    array))
(defparameter *virtual-window* (make-virtual-window))
(defparameter *virtual-window-lock* (bt:make-recursive-lock))
(defun set-virtual-window (x y value)
  (setf (aref (aref *virtual-window* y) x)
	value))

(defmacro with-virtual-window-lock (&body body)
  `(bt:with-recursive-lock-held (*virtual-window-lock*)
     ,@body))

(defparameter *queue* nil)

(deflazy event-queue ()
  (setf *queue* (lparallel.queue:make-queue)))

(deflazy virtual-window ((w application::w) (h application::h) (event-queue event-queue))
  (lparallel.queue:push-queue :resize event-queue)
  (setf *columns* (floor w *glyph-width*)
	*lines* (floor h *glyph-height*))
  (with-virtual-window-lock
    (setf *virtual-window* (make-virtual-window))))
#+nil
(defclass sprite ()
  ((bounding-box :accessor sprite.bounding-box
		 :initform (make-instance 'rectangle
					  :x0 -0.25 :y0 -0.25
					  :x1 0.25 :y1 0.25)
		 :initarg :bounding-box)
   (absolute-rectangle :accessor sprite.absolute-rectangle
		       :initform (make-instance 'rectangle)
		       :initarg :absolute-rectangle)
   (string :accessor sprite.string
	   :initform "Hello World"
	   :initarg :string)
   (tickfun :accessor sprite.tickfun
	    :initform nil
	    :initarg :tickfun)
   (onclick :accessor sprite.onclick
	    :initform nil
	    :initarg :onclick)
   (position :accessor sprite.position
	     :initform (make-instance 'point)
	     :initarg :position)))
#+nil
(defun closest-multiple (x n)
  (* n (round x n)))

(defparameter *mouse-x* 0.0)
(defparameter *mouse-y* 0.0)
#+nil
(defun random-point ()
  (make-instance 'point
		 :x (* *glyph-width* (random 80))
		 :y (* *glyph-height* (random 25))))
#+nil
(defun integer-point (x y)
  (make-instance 'point
		 :x (* *glyph-width* x)
		 :y (* *glyph-height* y)))
#+nil
(defun string-bounding-box (string &optional (rectangle (make-instance 'rectangle)))
  (multiple-value-bind (x y) (string-bounds string)
    (with-slots (x0 y0 x1 y1) rectangle
      (setf x0 0.0
	    y0 (- (* *glyph-height* y))
	    x1 (* *glyph-width* x)
	    y1 *glyph-height*))))
#+nil
(defun string-bounds (string)
  (let ((len (length string))
	(maxx 0)
	(x 0)
	(y 0))
    (dotimes (index len)
      (let ((char (aref string index)))
	(cond ((char= char #\Newline)
	       (when (> x maxx)
		 (setf maxx x))
	       (setf x 0)
	       (decf y))
	      (t
	       (setf x (1+ x))))))
    (values (max x maxx) y)))

#+nil
(progn
  (defparameter *selection* nil)
  (defparameter *hovering* nil)
  (defparameter *drag-offset-x* 0.0)
  (defparameter *drag-offset-y* 0.0))

(defparameter *last-scroll* 0)
(defparameter *scroll-difference* 0)
(defun init ())
(defun app ()
  (let ((newscroll (floor window::*scroll-y*)))
    (setf *scroll-difference* (- newscroll *last-scroll*))
    (setf *last-scroll* newscroll))
  #+nil
  (setf *mouse-x* (floatify window::*mouse-x*)
	*mouse-y* (- window::*height* (floatify window::*mouse-y*)))
  #+nil
  (progn
    (when (window::skey-j-p (window::keyval #\esc))
      (pop-sprite-chain-stack))
    (do-sprite-chain (sprite t) ()
      (let ((fun (sprite.tickfun sprite)))
	(when fun
	  (funcall fun))))
    (when 
	(window::skey-j-p (window::mouseval 4))
      (typecase *hovering*
	(sprite
	 (sprite-chain:remove-sprite *hovering*)
	 (setf *hovering* nil)))))

  #+nil
  (let ((mousex *mouse-x*)
	(mousey *mouse-y*))
      ;;search for topmost sprite to drag
    (let
	((sprite
	  (block cya
	    (do-sprite-chain (sprite) ()
	      (with-slots (absolute-rectangle) sprite
		(when (coordinate-inside-rectangle-p mousex mousey absolute-rectangle)
		  (return-from cya sprite)))))))
      (setf *hovering* sprite)
      (when sprite	
	(when (window::skey-j-p (window::mouseval :left))
	  (let ((onclick (sprite.onclick sprite)))
	    (when onclick
	      (funcall onclick sprite))))
	(when (window::skey-j-p (window::mouseval 5))
	  (with-slots (position) sprite
	    (with-slots (x y) position
	      (setf *drag-offset-x* (- x mousex)
		    *drag-offset-y* (- y mousey))))
	  (setf *selection* sprite)
	  (topify-sprite sprite))))
    (typecase *selection*
      (sprite (with-slots (x y) (slot-value *selection* 'position)
		(let ((xnew (closest-multiple (+ *drag-offset-x* mousex) *glyph-width*))
		      (ynew (closest-multiple (+ *drag-offset-y* mousey) *glyph-height*)))
		  (unless (eq x xnew)
		    (setf x xnew))
		  (unless (eq y ynew)
		    (setf y ynew)))))))
  #+nil
  (when (window::skey-j-r (window::mouseval 5))
    (setf *selection* nil))
  #+nil
  (do-sprite-chain (sprite t) ()
    (update-bounds sprite))
  

  (glhelp:set-render-area 0 0 window:*width* window:*height*)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  ;;(gl:clear :color-buffer-bit)
  (gl:polygon-mode :front-and-back :fill)
  (gl:disable :cull-face)
  (gl:disable :blend)

  (render-stuff))
#+nil
(defun update-bounds (sprite)
  (with-slots (bounding-box position absolute-rectangle)
      sprite
    (with-slots (x0 y0 x1 y1) bounding-box
      (with-slots ((xpos x) (ypos y)) position
	(let ((px0 (+ x0 xpos))
	      (py0 (+ y0 ypos))
	      (px1 (+ x1 xpos))
	      (py1 (+ y1 ypos)))
	  (with-slots (x0 y0 x1 y1) absolute-rectangle
	    (setf x0 px0 y0 py0 x1 px1 y1 py1)))))))

(progn
  (deflazy flat-shader-source ()
    (glslgen:ashader
     :vs
     (glslgen2::make-shader-stage
      :out '((value-out "vec4"))
      :in '((position "vec4")
	    (value "vec4")
	    (pmv "mat4"))
      :program
      '(defun "main" void ()
	(= "gl_Position" (* pmv position))
	(= value-out value)))
     :frag
     (glslgen2::make-shader-stage
      :in '((value "vec4"))
      :program
      '(defun "main" void ()
	(=
	 :gl-frag-color
	 value
	 )))
     :attributes
     '((position . 0) 
       (value . 3))
     :varyings
     '((value-out . value))
     :uniforms
     '((:pmv (:vertex-shader pmv)))))
  (deflazy flat-shader (flat-shader-source gl-context)
    (glhelp::create-gl-program flat-shader-source)))

#+nil
(defun bytecolor (r g b &optional (a 3))
  "each channel is from 0 to 3"
  (byte/255		    
   (text-sub::color-rgba r g b a)
   ))

#+nil
(defun draw-string
    (x y string &optional
		  (fgcol
		   (bytecolor 0 0 0 3
		    ))
		  (bgcol		   
		   (bytecolor 3 3 3 3)
		    ))
  (let ((start x)
	(len (length string)))
    (dotimes (index len)
      (let ((char (aref string index)))
	(cond ((char= char #\Newline)
	       (setf x start)
	       (decf y))
	      (t
	       (color (byte/255 (char-code char))
		      bgcol
		      fgcol)
	       (vertex (floatify x)
		       (floatify y)
		       0.0)			  
	       (incf x)))))))

(defun render-stuff ()
  (;;text-sub::with-data-shader (uniform rebase)
   ;; (gl:clear :color-buffer-bit)
 ;;   (gl:disable :depth-test)

    ;;"sprites"
    #+nil
    (do-sprite-chain (sprite t) ()
      (with-slots (position string)
	  sprite
	(with-slots ((xpos x) (ypos y)) position
	  (multiple-value-bind (fgcolor bgcolor) 
	    (cond ((eq sprite *selection*)
		   (values
		    (bytecolor 3 0 0 3)
		    (bytecolor 0 3 3 0)))
		  ((eq sprite *hovering*)
		   (values
		    (bytecolor 0 0 0)
		    (bytecolor 3 3 3)))
		  (t
		   (values
		    (bytecolor 3 3 3)
		    (bytecolor 0 0 0))))
	    (draw-string (/ xpos *glyph-width*)
			 (/ ypos *glyph-height*)
			 string
			 fgcolor
			 bgcolor)))))
    ;;   #+nil
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
  (when *update-p*
    (setf *update-p* nil)
    ;;;Copy the virtual screen to a c-array,
    ;;;then send the c-array to an opengl texture
    (let* ((c-array-lines (+ 1 *lines*))
	   (c-array-columns (+ 1 *columns*))
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
	 
	 (let ((len *lines*))
	   (dotimes (i len)
	     (let ((array (aref *virtual-window* (- len i 1))))
	       (dotimes (index *columns*)
		 (let* ((glyph (aref array index))
			(attributes (glyph-attributes glyph))
			(pair (ncurses-color-pair (mod attributes 256))))
		   (flet ((byte/255 (n)
			    (identity n)))
		     (let ((realfg
			    (let ((fg (car pair)))
			      (if (or
				   (not pair)
				   (= -1 fg))
				  (byte/255
				   *fg-default*) ;;FIXME :cache?
				  (byte/255
				   fg))))
			   (realbg
			    (let ((bg (cdr pair)))
			      (if (or
				   (not pair)
				   (= -1 bg))
				  (byte/255
				   *bg-default*) ;;FIXME :cache?
				  (byte/255
				   bg)))))
		       (when (logtest A_reverse attributes)
			 (rotatef realfg realbg))
		       (color (byte/255
			       (char-code (glyph-value glyph)))
			      realfg
			      realbg
			      (byte/255
			       (logior (if (logtest A_Underline attributes)
					   1
					   0)
				       (if (logtest A_bold attributes)
					   2
					   0)))
			      index
			      i))
		     #+nil
		     (vertex (floatify x)
			     (floatify y)
			     0.0))
		   #+nil
		   (incf x)))))))
       (let* ((framebuffer (getfnc 'text-sub::text-data))
	      (texture (glhelp::texture framebuffer)))
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
    (glhelp:set-render-area 0 0 (getfnc 'application::w) (getfnc 'application::h))
    #+nil
    (progn
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))

    (gl:call-list (glhelp::handle (getfnc 'text-sub::fullscreen-quad)))))

#+nil
(defun plain-button (fun &optional
			   (str (string (gensym "nameless-button-")))
			   (pos (random-point))
			   (sprite (make-instance 'sprite)))
  "a statically named button"
  (let ((rect (make-instance 'rectangle)))
    (string-bounding-box str rect)
    (with-slots (position bounding-box string onclick) sprite
      (setf position pos
	    bounding-box rect
	    string str
	    onclick fun)))
  sprite)

#+nil
(progn
  (defparameter *sprite-chain-stack* nil)
  (defparameter *sprite-chain-stack-depth* 0)
  (defun push-sprite-chain-stack (&optional (new-top (sprite-chain:make-sprite-chain)))
    (push sprite-chain::*sprites* *sprite-chain-stack*)
    (setf sprite-chain::*sprites* new-top)
    (incf *sprite-chain-stack-depth*))
  (defun pop-sprite-chain-stack ()
    (let ((top (pop *sprite-chain-stack*)))
      (when top
	(decf *sprite-chain-stack-depth*)
	(setf sprite-chain::*sprites* top))))
  (defun replace-sprite-chain-stack ()
    (pop-sprite-chain-stack)
    (push-sprite-chain-stack)))
#+nil
(defun bottom-layer ()
  #+nil
  (add-sprite
   (plain-button
    (lambda (this) (remove-sprite this))
    "hello world"))
  (add-sprite
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (application::quit))
    "quit"
    (integer-point 0 1)))
  #+nil
  (add-sprite
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (new-layer))
    "new"))
  #+nil
  (let ((rect (make-instance 'rectangle))
	(numbuf (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character)))
    (add-sprite
     (make-instance
      'sprite
      :position (integer-point 10 1)
      :bounding-box rect
      :tickfun
      (lambda ()
	;;mouse coordinates
	(setf (fill-pointer numbuf) 0)
	(with-output-to-string (stream numbuf :element-type 'character)
	  #+nil
	  (princ (list (floor *mouse-x*)
		       (floor *mouse-y*))
		 stream)
	  (princ (aref block-data::*names*
		       testbed::*blockid*)
		 stream)
	  )
	(string-bounding-box numbuf rect))
      :string numbuf
      ))))
#+nil
(defun new-layer ()
  (push-sprite-chain-stack)
  (add-sprite 
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (new-layer))
    "new"))
  (add-sprite
   (plain-button
    (lambda (this)
      (declare (ignorable this))
      (pop-sprite-chain-stack))
    "back"))
  (add-sprite
   (plain-button
    nil
    (format nil "layer ~a" *sprite-chain-stack-depth*))))
#+nil
(progn
  (setf sprite-chain::*sprites* (sprite-chain:make-sprite-chain))
  (bottom-layer))

(defparameter *fg-default-really* 15)
(defparameter *bg-default-really* 0)

(defparameter *fg-default* *fg-default-really*)
(defparameter *bg-default* *bg-default-really*)

(defparameter *pairs* (let ((pairs (make-hash-table)))
			(setf (gethash 0 pairs)
			      (cons *fg-default-really*
				    *bg-default-really*)
 ;;;;FIXME whats white and black for default? short?
			      )
			pairs))

(defun ncurses-init-pair (pair-counter fg bg)
  (setf (gethash pair-counter *pairs*)
	(cons fg bg)))
(defun ncurses-color-pair (pair-counter)
  (gethash pair-counter *pairs*)) ;;fixme -> this is not how ncurses works.

(defun ncurses-pair-content (pair-counter)
  (let ((pair (ncurses-color-pair pair-counter)))
    (values (car pair)
	    (cdr pair))))

(defun ncurses-assume-default-color (fg bg)
  ;;;;how ncurses works. see https://users-cs.au.dk/sortie/sortix/release/nightly/man/man3/assume_default_colors.3.html
  (setf *fg-default* (if (= fg -1)
			 *fg-default*
			 fg)
	*bg-default* (if (= bg -1)
			 *bg-default*
			 bg))
  (ncurses-init-pair 0 *fg-default* *bg-default*))

(defparameter *ncurses-windows* (make-hash-table))
(defun add-win (win)
   (setf (gethash win *ncurses-windows*)
	t))
(defun remove-win (win)
  (remhash win *ncurses-windows*))

(struct-to-clos:struct->class
 (defstruct win
   lines
   COLS
   y
   x
   keypad-p ;;see https://linux.die.net/man/3/keypad
   clearok
   scrollok
   attr-bits
   cursor-y
   cursor-x
   data))

(set-pprint-dispatch 'win 'print-win)
(defun print-win (stream win)
  (format stream "lines: ~a cols: ~a" (win-lines win) (win-cols win))
  (print-grid (win-data win) stream (win-cursor-x win) (win-cursor-y win)))

;;window is an array of lines, for easy swapping and scrolling of lines. optimizations later
(defun make-row (width)
  (make-array width :initial-element *clear-glyph*))
(defun make-grid (rows columns)
  (let ((rows-array (make-array rows)))
    (dotimes (i rows)
      (setf (aref rows-array i)
	    (make-row columns)))
    rows-array))

(defun grid-rows (grid)
  (length grid))
(defun grid-columns (grid)
  (length (aref grid 0)))
(utility::etouq
  (let ((place '(aref (aref grid y) x))
	(args '(x y grid)))
    `(progn
       (defun ref-grid (,@args)
	 ,place)
       (defun (setf ref-grid) (new ,@args)
	 (setf ,place new)
	 new))))

(defun print-grid (grid &optional (stream *standard-output*) (cursor-x 0) (cursor-y 0))
  (dotimes (grid-row (grid-rows grid))
    (terpri stream)
    (write-char #\| stream)
    (let ((row-data (aref grid grid-row)))	
      (dotimes (grid-column (grid-columns grid)) ;;FIXME dereferencing redundancy
	(let ((cursor-here-p (and (= grid-column cursor-x)
				  (= grid-row cursor-y)))
	      (x (aref row-data grid-column)))
	  (when cursor-here-p (write-char #\[ stream))
	  (write-char 
	   (typecase x
	     (glyph (glyph-value x))
	     (t #\space))
	   stream)
	  (when cursor-here-p (write-char #\] stream)))))
    (write-char #\| stream))
  (terpri stream)
  grid)

(defun move-row (old-n new-n grid)
  "move row old-n to new-n"
  (cond ((> (grid-rows grid) new-n -1)
	 (setf (aref grid new-n)
	       (aref grid old-n))
	 (setf (aref grid old-n) nil))
	(t (error "moving to a row that does not exist")))
  grid)

(defun transfer-data (grid-src grid-dest)
  (let ((shared-rows
	 (min (grid-rows grid-src)
	      (grid-rows grid-dest)))
	(shared-columns
	 (min (grid-columns grid-src)
	      (grid-columns grid-dest))))
    (dotimes (row-index shared-rows)
      ;;FIXME optimization? can cache the row. but its a fragile optimization
      (dotimes (column-index shared-columns)
	(setf (ref-grid column-index row-index grid-dest)
	      (ref-grid column-index row-index grid-src)))))
  grid-dest)

(defparameter *win* nil)

(defun ncurses-newwin (nlines ncols begin-y begin-x)
  (let ((win (make-win :lines nlines
		       :cols ncols
		       :y begin-y
		       :x begin-x
		       :cursor-x 0
		       :cursor-y 0
		       :data (make-grid nlines ncols)
		       :attr-bits 0)))
  ;;  (add-win win)
    (setf *win* win)
    win))

(defparameter *std-scr* (ncurses-newwin 25 80 0 0))

(defun ncurses-move (y x)
  (ncurses-wmove *std-scr* y x))

(defun ncurses-vline (char n)
  (ncurses-wvline *std-scr* char n))
;;https://www.mkssoftware.com/docs/man3/curs_border.3.asp
(defun ncurses-wvline (win char n)
  (let ((y (win-cursor-y win))
	(x (win-cursor-x win)))
    (loop :for i :from y :below (min (+ y n)
				     (win-lines win))
       :do (add-char x i char))))

(defun ncurses-keypad (win value)
  (setf (win-keypad-p win) value))
(defun ncurses-delwin (win)
  (remove-win win))

(defun c-true (value)
  (not (zerop value)))

(defun ncurses-clearok (win value)
  "If clearok is called with TRUE as argument, the next call to wrefresh with this window will clear the screen completely and redraw the entire screen from scratch. This is useful when the contents of the screen are uncertain, or in some cases for a more pleasing visual effect. If the win argument to clearok is the global variable curscr, the next call to wrefresh with any window causes the screen to be cleared and repainted from scratch. "
  (setf (win-clearok win)
	(c-true value)))

;;;FIXME add default window for ncurses like stdscr

(defun ncurses-mvwin (win y x)
  "Calling mvwin moves the window so that the upper left-hand corner is at position (x, y). If the move would cause the window to be off the screen, it is an error and the window is not moved. Moving subwindows is allowed, but should be avoided."
  ;;;FIXME: detect off screen 
  (setf (win-x win) x
	(win-y win) y))

(defun ncurses-wresize (win height width)
  (setf (win-lines win) height
	(win-cols win) width)
  (let ((old-data (win-data win))
	(new-grid (make-grid height width)))
    (transfer-data old-data new-grid)
    (setf (win-data win)
	  new-grid)))

(defparameter *mouse-enabled-p* nil)

(defun ncurses-wattron (win attr)
  (let ((old (win-attr-bits win)))
    (setf (win-attr-bits win)
	  (logior attr old))))

(defun ncurses-wattroff (win attr)
  (let ((old (win-attr-bits win)))
    (setf (win-attr-bits win)
	  (logand (lognot attr) old))))

;;(defun ncurses-wscrl (win n))
;;https://linux.die.net/man/3/scrollok
(defun ncurses-wmove (win y x)
  (setf (win-cursor-x win) x
	(win-cursor-y win) y))

(defparameter *cursor-state* :normal)
(defun ncurses-curs-set (value)
  "The curs_set routine sets the cursor state is set to invisible, normal, or very visible for visibility equal to 0, 1, or 2 respectively. If the terminal supports the visibility requested, the previous cursor state is returned; otherwise, ERR is returned."
  (setf *cursor-state*
	(case value
	  (0 :invisible)
	  (1 :normal)
	  (2 :very-visible))))

(defparameter A_BOLD
  #b100000000 ;;8 bits for char, could be 7?
  ;;#x00200000
  )
(defparameter A_UNDERLINE
  #b1000000000
  ;;#x00020000
  )
(defparameter A_REVERSE
  #b10000000000
  )

(defun %ncurses-wscrl (grid n)
  (let ((width (grid-columns grid)))
    (cond ((plusp n)
	   ;;scrolling up means lines get moved up,
	   ;;which means start at top of screen to move, which is smallest.
	   (loop :for i :from n :below (grid-rows grid)
	      :do
	      (move-row i
			(- i n)
			grid)))
	  ((minusp n)
	   ;;scrolling down means lines get moved down,
	   ;;which means start at bottom of screen to move, which is largest.
	   (let ((move-distance (- n)))
	     (loop :for i :from (- (grid-rows grid) 1 move-distance) :downto 0
		:do
		(move-row i
			  (+ i move-distance)
			  grid))))
	  ((zerop n) t))
    ;;;;fill in those nil's. OR FIXME
    (map-into grid (lambda (x) (or x (make-row width))) grid))
  grid)
(defun ncurses-wscrl (win n)
  (%ncurses-wscrl (win-data win) n))


(defun ncurses-mvwaddstr (win y x string)
  (setf (win-cursor-x win) x
	(win-cursor-y win) y)
  (ncurses-waddstr win string))
(defun ncurses-waddstr (win string)
  (dotimes (index (length string))
    (ncurses-waddch win (aref string index))))
(defun ncurses-wclrtoeol (&optional (win *win*))
  "The clrtoeol() and wclrtoeol() routines erase the current line to the right of the cursor, inclusive, to the end of the current line. https://www.mkssoftware.com/docs/man3/curs_clear.3.asp"
  (let ((x (win-cursor-x win))
	(y (win-cursor-y win)))
    (loop :for i :from x :below (win-cols win)
       :do (add-char i y #\Space win)))
  win)
(defun ncurses-wclrtobot (&optional (win *win*))
  "The clrtobot() and wclrtobot() routines erase from the cursor to the end of screen. That is, they erase all lines below the cursor in the window. Also, the current line to the right of the cursor, inclusive, is erased. https://www.mkssoftware.com/docs/man3/curs_clear.3.asp"
  (ncurses-wclrtoeol win)
  (let ((y (win-cursor-y win)))
    (loop :for i :from (+ y 1) :below (win-lines win)
       :do
       (loop :for z :from 0 :below (win-cols win)
	  :do (add-char z i #\Space win))))
  win)

(defun max-cursor-y (&optional (win *win*))
  "the greatest value a cursor's y pos can be"
  (1- (win-lines win)))
(defun max-cursor-x (&optional (win *win*))
  "the greatest value a cursor's x pos can be"
  (1- (win-cols win)))

(defun ncurses-waddch (win char)
  " The addch(), waddch(), mvaddch() and mvwaddch() routines put the character ch into the given window at its current window position, which is then advanced. They are analogous to putchar() in stdio(). If the advance is at the right margin, the cursor automatically wraps to the beginning of the next line. At the bottom of the current scrolling region, if scrollok() is enabled, the scrolling region is scrolled up one line.

If ch is a tab, newline, or backspace, the cursor is moved appropriately within the window. Backspace moves the cursor one character left; at the left edge of a window it does nothing. Newline does a clrtoeol(), then moves the cursor to the window left margin on the next line, scrolling the window if on the last line). Tabs are considered to be at every eighth column. https://www.mkssoftware.com/docs/man3/curs_addch.3.asp If ch is any control character other than tab, newline, or backspace, it is drawn in ^X notation. Calling winch() after adding a control character does not return the character itself, but instead returns the ^-representation of the control character. (To emit control characters literally, use echochar().) "
  (let ((x (win-cursor-x win))
	(y (win-cursor-y win)))
    (flet ((advance ()	     
	     (if (= (max-cursor-x win) x)
		 (if (= (max-cursor-y win) y)
		     (cond ((win-scrollok win) ;;scroll the window and reset to x pos
			    (ncurses-wscrl win 1)
			    (setf (win-cursor-x win) 0))
			   (t (progn ;;do nothing
				)))
		     ;;reset x and go to next line, theres space
		     (setf (win-cursor-x win) 0
			   (win-cursor-y win) (+ 1 y)))
		 ;;its not at the end of line, no one cares
		 (setf (win-cursor-x win) (+ 1 x)))))
      (cond 
	((char= char #\tab)
	 (setf (win-cursor-x win)
	       (next-8 x)))
	((char= char #\newline)
	 (ncurses-clrtoeol)
	 (let ((max-cursor-y (max-cursor-y win)))
	   (if (= max-cursor-y y)
	       (ncurses-wscrl win 1)
	       (setf (win-cursor-y win)
		     (min (+ 1 y)
			  max-cursor-y))))
	 (setf (win-cursor-x win) 0))
	((char= char #\backspace)
	 (setf (win-cursor-x win)
	       (max 0 (- x 1))))
	((char-control char)
	 (add-char x y #\^ win)
	 (advance)
	 (ncurses-waddch win (char-control-printable char)))
	((standard-char-p char)
	 (add-char x y char win)
	 (advance))
	(t (error "what char? ~s" (char-code char)))))))

(defun next-8 (n)
  "this is for tabbing, see waddch. its every 8th column"
  (* 8 (+ 1 (floor n 8))))

(defun add-char (x y value &optional (win *win*))
  (when (and (> (win-lines win) y -1)
	     (> (win-cols win) x -1))
    (setf (ref-grid x y (win-data win))
	  (gen-glyph value
		     (logior (win-attr-bits win)
			     *current-attributes*))))
  win)

;;https://invisible-island.net/ncurses/ncurses-intro.html#stdscr
#+nil "The other is to set the current-highlight value. This is logical-or'ed with any highlight you specify the first way. You do this with the functions attron(), attroff(), and attrset(); see the manual pages for details. Color is a special kind of highlight. The package actually thinks in terms of color pairs, combinations of foreground and background colors. The sample code above sets up eight color pairs, all of the guaranteed-available colors on black. Note that each color pair is, in effect, given the name of its foreground color. Any other range of eight non-conflicting values could have been used as the first arguments of the init_pair() values."
(defparameter *current-attributes* 0)
(defun ncurses-attron (n)
  (setf *current-attributes*
	(logior *current-attributes* n)))
(defun ncurses-attroff (n)
  (setf *current-attributes*
	(logand *current-attributes* (lognot n))))

(defun char-control (char)
  ;;FIXME: not portable common lisp, requires ASCII
  (let ((value (char-code char)))
	(if (> 32 value)
	    t
	    nil)))

(defun char-control-printable (char)
  ;;FIXME: not portable common lisp, requires ASCII
  (code-char (logior 64 (char-code char))))

(defun fuzz (&optional (win *win*))
  (dotimes (x 100)
    (add-char (random (win-cols win))
	      (random (win-lines win))
	      #\a
	      win))
  win)

(defparameter *no* *standard-output*)

(defun ncurses-wnoutrefresh (&optional (win *win*))
  ;;;FIXME:: follow https://linux.die.net/man/3/wnoutrefresh with "touching"
  ;;;different lines
  (when (win-clearok win)
    ;;FIXME -> clearok? what to do? check this: https://linux.die.net/man/3/clearok
    (setf (win-clearok win) nil))
  (with-virtual-window-lock
    (let ((grid (win-data win))
	  (xwin (win-x win))
	  (ywin (win-y win))
	  ;;(cursor-x (win-cursor-x win))
	  ;;(cursor-y (win-cursor-y win))
	  (columns (length (aref *virtual-window* 0)))
	  (lines (length *virtual-window*)))
      (dotimes (y (win-lines win))
	(dotimes (x (win-cols win))
	  (let ((glyph (ref-grid x y grid)))
	    (let ((xdest (+ xwin x))
		  (ydest (+ ywin y)))
	      (when (and (> columns xdest -1)
			 (> lines ydest -1))
		#+nil
		(if (or (= cursor-x x)
			(= y cursor-y)
			 )
		    (setf glyph (logior glyph A_Reverse))) 
		(set-virtual-window xdest
				    ydest
				    glyph
				    )))))))))

(defparameter *update-p* nil)
(defun ncurses-doupdate ()
  (setf *update-p* t)) ;;;when copied to opengl buffer, set again to nil

(defun coerce-to-char (x)
  (typecase x
    (number (code-char x))
    (character x)))
(defun print-virtual-window (&optional (array *virtual-window*) (stream *standard-output*))
  (with-virtual-window-lock
    (let ((horizontal-bar (+ 2 (length (aref array 0)))))
      (terpri stream)
      (dotimes (i horizontal-bar) (write-char #\_ stream))
      (dotimes (line (length array))
	(terpri stream)
	(write-char #\| stream)
	(let ((line-array (aref array line)))
	  (dotimes (i (length line-array))
	    (write-char (coerce-to-char (glyph-value (aref line-array i))) stream)))
	(write-char #\| stream))
      (terpri stream)
      (dotimes (i horizontal-bar) (write-char #\_ stream))
      (terpri stream))))


#+nil
(let ((program (getfnc 'flat-shader)))
  (glhelp::use-gl-program program)
  (glhelp:with-uniforms uniform program
    (gl:uniform-matrix-4fv (uniform :pmv)
			   (nsb-cga:matrix*
			    (nsb-cga:scale*
			     (/ 2.0 (floatify window::*width*))
			     (/ 2.0 (floatify window::*height*))
			     1.0)
			    (nsb-cga:translate* 
			     (/ (floatify window::*width*)
				-2.0)				 
			     (/ (floatify window::*height*)
				-2.0)
			     0.0))
			   nil)))
#+nil
(progn
  (do-sprite-chain (sprite t) ()
    (render-sprite sprite))
  (gl:with-primitive :quads
    (mesh-vertex-tex-coord-color)))

#+nil
(defparameter *pen-color* (list 1.0 0.0 0.0 1.0))

#+nil
(defun render-sprite (sprite)
  (with-slots (absolute-rectangle)
      sprite
    (let ((*pen-color*
	   (cond ((eq sprite *selection*)
		  '(1.0 0.0 0.0 1.0))
		 ((eq sprite *hovering*)
		  '(0.0 0.0 0.0 1.0))
		 (t
		  '(1.0 1.0 1.0 1.0)))))
      (with-slots (x0 y0 x1 y1) absolute-rectangle
	(draw-quad x0 y0 
		   x1 y1)))))

#+nil
(defun render-tile (char-code x y background-color foreground-color)
  (color (byte/255 char-code)
	 (byte/255 background-color)
	 (byte/255 foreground-color))
  (vertex
   (floatify x)
   (floatify y)))
#+nil
;;a rainbow
(let ((count 0))
  (dotimes (x 16)
    (dotimes (y 16)
      (render-tile count x y count (- 255 count))
      (incf count))))

;;;more geometry
#+nil
(defun draw-quad (x0 y0 x1 y1)
  (destructuring-bind (r g b a) *pen-color*
    (color r g b a)
    (vertex x0 y0)
    (color r g b a)
    (vertex x0 y1)
    (color r g b a)
    (vertex x1 y1)
    (color r g b a)
    (vertex x1 y0)))
