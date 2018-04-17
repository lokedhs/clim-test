(defpackage :freetype-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :freetype-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (unless (find-package "LOG4CL")
      (ql:quickload "log4cl"))
    (ql:quickload "mcclim")))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defmacro dimension-bind ((output-record &key
                                           ((:width width-sym)) ((:height height-sym))
                                           ((:x x-sym)) ((:y y-sym))
                                           ((:right right-sym)) ((:bottom bottom-sym))
                                           ((:baseline baseline-sym)))
                          &body body)
  (alexandria:with-gensyms (width height x y)
    (alexandria:once-only (output-record)
      (labels ((make-body ()
                 `(progn ,@body))
               (make-baseline ()
                 (if baseline-sym
                     `(let ((,baseline-sym (clim-extensions:output-record-baseline ,output-record)))
                        ,(make-body))
                     (make-body)))
               (make-position-form ()
                 (if (or x-sym y-sym right-sym bottom-sym)
                     `(multiple-value-bind (,x ,y)
                          (clim:output-record-position ,output-record)
                        (declare (ignorable ,x ,y))
                        (let (,@(if x-sym `((,x-sym ,x)))
                              ,@(if y-sym `((,y-sym ,y)))
                              ,@(if right-sym `((,right-sym (+ ,x ,width))))
                              ,@(if bottom-sym `((,bottom-sym (+ ,y ,height)))))
                          ,(make-baseline)))
                     (make-baseline))))
        (if (or width-sym height-sym right-sym bottom-sym)
            `(multiple-value-bind (,width ,height)
                 (clim:rectangle-size ,output-record)
               (declare (ignorable ,width ,height))
               (let (,@(if width-sym `((,width-sym ,width)))
                     ,@(if height-sym `((,height-sym ,height))))
                 ,(make-position-form)))
            (make-position-form))))))

(defclass text-content-view (clim:view)
  ())

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content
                        :redisplay-on-resize-p t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (let ((string (format nil "Office"))
        (s (clim:make-text-style "Caladea" "Italic" 40)))
    (clim:with-text-style (stream s)
      (let ((x 60)
            (y 100))
        (clim:draw-text* stream string x y :ink clim:+black+)
        (clim:draw-line* stream 20 y 400 y :ink clim:+red+)
        (multiple-value-bind (width height cursor-x cursor-y baseline)
            (clim:text-size stream string)
          (log:info "width=~s height=~s cursor-x=~s cursor-y=~s baseline=~s" width height cursor-x cursor-y baseline)
          (clim:draw-rectangle* stream x y (+ x width) (- y height) :filled nil :ink clim:+blue+))))))

(defmethod clim-clx::font-draw-glyphs :around ((font clim-freetype::freetype-font) mirror gc x y string
                                               &key (start 0) (end (length string))
                                                 translate size (direction :ltr))
  (declare (ignore translate size))
  (log:info "Intercepting: ~s" string)
  (loop
    with parts = (mcclim-bidi:directions (subseq string start end) (ecase direction
                                                                     (:ltr nil)
                                                                     (:rlt t)))
    with dx = x
    for (part-direction . s) in parts
    do (multiple-value-bind (width ascent descent left right font-ascent font-descent dir first-not-done)
           (clim-clx::font-text-extents font s)
         (declare (ignore width ascent descent left font-ascent font-descent dir first-not-done))
         (log:info "Drawing part at: ~f (next skip: ~f)" dx right)
         (call-next-method font mirror gc dx y s :direction part-direction)
         (incf dx right))))
