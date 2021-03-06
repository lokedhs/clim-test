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
  (let ((frame (clim:make-application-frame 'foo-frame :width 700 :height 800)))
    (clim:run-frame-top-level frame)))

(defun draw-x (stream x y &optional (size 5))
  (clim:draw-line* stream (- x size) (- y size) (+ x size) (+ y size))
  (clim:draw-line* stream (- x size) (+ y size) (+ x size) (- y size)))

(defun draw-rotated-text (stream)
  (let ((text "abcdefgh")
        (x 40)
        (y 60))
    (clim:with-drawing-options (stream :transformation (clim:compose-translation-with-transformation 
                                                        (clim:make-rotation-transformation* 0.4 x y)
                                                        100 100)
                                       :text-style (clim:make-text-style "DejaVu Sans" "Regular" 72))
      (clim:draw-text* stream text x y)
      (multiple-value-bind (width height w baseline ascent)
          (clim:text-size stream text)
        (declare (ignore w baseline))
        (clim:draw-rectangle* stream x (- y ascent) (+ x width) (+ y (- height ascent))
                              :filled nil :ink clim:+red+)))))

(defun draw-simple-text (stream)
  (let ((text "FoojA")
        (x 40)
        (y 40)
        (s (clim:make-text-style "DejaVuSans" "Book" 20)))
    (clim:with-text-style (stream s)
      (clim:draw-text* stream text x y :ink clim:+black+)
      (multiple-value-bind (width height w baseline ascent)
          (clim:text-size stream text)
        (declare (ignorable width height w baseline ascent))
        (clim:draw-rectangle* stream x (- y ascent) (+ x width) (+ y (- height ascent))
                              :filled nil :ink clim:+red+))
      ;;
      (let ((rec (clim:with-output-to-output-record (stream)
                   (clim:draw-text* stream text 0 0 :ink clim:+black+))))
        (setf (clim:output-record-position rec)
              (values 60 200))
        (clim:stream-add-output-record stream rec)
        (dimension-bind (rec :x x1 :y y1 :right x2 :bottom y2)
          (clim:draw-rectangle* stream x1 y1 x2 y2 :filled nil :ink clim:+blue+))
        (draw-x stream 60 200)))))

(defun draw-transform-box (stream)
  (let ((rec (clim:with-output-to-output-record (stream)
               (clim:with-drawing-options (stream :transformation (clim:compose-translation-with-transformation
                                                                   (clim:make-rotation-transformation* 0.4 50 100)
                                                                   10 20)
                                                  :text-style (clim:make-text-style "Noto Sans Devanagari" "Book" 12))
                 #+nil(clim:draw-rectangle* stream 50 100 200 150 :filled nil)
                 (clim:draw-text* stream "यह CLIM में हिंदी का एक परीक्षण है" 50 100 :text-size 48)))))
    (clim:stream-add-output-record stream rec)
    (dimension-bind (rec :x x1 :y y1 :right x2 :bottom y2)
      (log:info "dimensions from dimension-bind: (~f,~f)-(~f,~f)" x1 y1 x2 y2)
      (clim:draw-rectangle* stream x1 y1 x2 y2 :filled nil :ink clim:+green+))
    (clim:replay rec stream)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  #+nil
  (draw-simple-text stream)
  (draw-transform-box stream)
  #+nil
  (let ((rec (clim:with-output-to-output-record (stream)
               (draw-rotated-text stream))))
    (clim:stream-add-output-record stream rec)
    (clim:replay (clim:stream-output-history stream) stream)
    (dimension-bind (rec :x x1 :y y1 :right x2 :bottom y2)
      (log:info "dimensions from dimension-bind: (~f,~f)-(~f,~f)" x1 y1 x2 y2)
      (clim:draw-rectangle* stream x1 y1 x2 y2 :filled nil :ink clim:+green+))))

(defmethod clim-clx::font-draw-glyphs :around ((font clim-freetype::freetype-font) mirror gc x y string
                                               &key (start 0) (end (length string))
                                                 translate size (direction :ltr) transformation)
  (loop
    with parts = (mcclim-bidi:directions (subseq string start end) (ecase direction
                                                                     (:ltr nil)
                                                                     (:rlt t)))
    with dx = x
    for (part-direction . s) in parts
    do (multiple-value-bind (width ascent descent left right font-ascent font-descent dir first-not-done)
           (clim-clx::font-text-extents font s)
         (declare (ignore width ascent descent left font-ascent font-descent dir first-not-done))
         (call-next-method font mirror gc dx y s
                           :direction part-direction :transformation transformation
                           :translate translate :size size)
         (incf dx right))))

(defmethod clim-clx::font-text-extents :around ((font clim-freetype::freetype-font) string
                                                &key (start 0) (end (length string))
                                                  translate (direction :ltr))
  (loop
    with parts = (mcclim-bidi:directions (subseq string start end) (ecase direction
                                                                     (:ltr nil)
                                                                     (:rlt t)))
    with dx = 0
    with max-ascent = 0
    with max-descent = 0
    with max-font-ascent = 0
    with max-font-descent = 0
    for (part-direction . s) in parts
    do (progn
         (multiple-value-bind (width ascent descent left right font-ascent font-descent)
             (call-next-method font s
                               :direction part-direction
                               :translate translate)
           (declare (ignore width left))
           (setq max-ascent (max max-ascent ascent))
           (setq max-descent (max max-descent descent))
           (setq max-font-ascent (max max-font-ascent font-ascent))
           (setq max-font-descent (max max-font-descent font-descent))
           (incf dx right)))
    finally (return (values dx max-ascent max-descent 0 dx max-font-ascent max-font-descent 0 end))))
