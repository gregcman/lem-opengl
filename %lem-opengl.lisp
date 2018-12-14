(in-package :%lem-opengl)

(defparameter *glyph-height* 16.0)
(defparameter *glyph-width* 8.0)

(defparameter *mouse-x* 0.0)
(defparameter *mouse-y* 0.0)

(defparameter *last-scroll* 0)
(defparameter *scroll-difference* 0)
(defun per-frame ()
  (let ((newscroll (floor window::*scroll-y*)))
    (setf *scroll-difference* (- newscroll *last-scroll*))
    (setf *last-scroll* newscroll))

  (glhelp:set-render-area 0 0 window:*width* window:*height*)
  ;;(gl:clear-color 0.0 0.0 0.0 0.0)
  ;;(gl:clear :color-buffer-bit)
  (gl:polygon-mode :front-and-back :fill)
  (gl:disable :cull-face)
  (gl:disable :blend)

  (render-stuff)
  )

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
  (when *update-p*
    (setf *update-p* nil)
    ;;;Copy the virtual screen to a c-array,
    ;;;then send the c-array to an opengl texture
    (let* ((c-array-lines
	    (min text-sub::*text-data-height* ;do not send data larger than text data
		 (+ 1 *lines*)))              ;width or height
	   (c-array-columns
	    (min text-sub::*text-data-width*
		 (+ 1 *columns*)))
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
		       ;;#+nil
		       (when (logtest A_reverse attributes)
			 (rotatef realfg realbg))
		       (color (byte/255
			       (char-code (glyph-value glyph)))
			      realfg
			      realbg
			      (byte/255
			       (text-sub::char-attribute
				(logtest A_Underline attributes)
				(logtest A_bold attributes)
				t))
			      index
			      i))
		     #+nil
		     (vertex (floatify x)
			     (floatify y)
			     0.0))
		   #+nil
		   (incf x)))))))
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
    (glhelp:set-render-area 0 0 (getfnc 'application::w) (getfnc 'application::h))
    ;#+nil
    (progn
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))

    (glhelp::slow-draw (getfnc 'text-sub::fullscreen-quad))))

(defparameter *queue* nil)

(deflazy event-queue ()
  (setf *queue* (lparallel.queue:make-queue)))

(deflazy virtual-window ((w application::w) (h application::h) (event-queue event-queue))
  (lparallel.queue:push-queue :resize event-queue)
  (setf *columns* (floor w *glyph-width*)
	*lines* (floor h *glyph-height*))
  (with-virtual-window-lock
    (setf *virtual-window* (make-virtual-window))))
