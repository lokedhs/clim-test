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
    (ql:quickload "mcclim")
    (ql:quickload "cl-freetype2")))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defvar *face* nil)

(defclass text-content-view (clim:view)
  ())

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content
                        :redisplay-on-resize-p t)
          #+nil(interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       #+nil interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (unless *face*
      (setq *face* (freetype2:new-face #p"/home/elias/.fonts/NotoSans-Regular.ttf"))
      (freetype2:set-char-size *face* (* 18 64) 0 72 72))
    (clim:run-frame-top-level frame)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (let* ((face (make-instance 'freetype-face :face *face*))
         (s (clim-internals::make-text-style (clim-extensions:font-face-family face) face 40)))
    (clim:draw-text* stream "Foo" 40 40 :text-style s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clx-freetype implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun log-and-sync (drawable msg &rest args)
  (xlib:display-finish-output (xlib:drawable-display drawable))
  (apply #'format *debug-io* msg args)
  (terpri *debug-io*))

(defparameter *freetype-font-scale* 26.6)

(defclass clx-freetype-port (clim-clx::clx-port)
  ())

(setf (get :clx-freetype :server-path-parser) 'clim-clx::parse-clx-server-path)
(setf (get :clx-freetype :port-type) 'clx-freetype-port)

(defvar *font-families* (make-hash-table :test 'equal))

(defclass freetype-font-family (clim-extensions:font-family)
  ())

(defun find-font-family (port name)
  (alexandria:ensure-gethash (list port name) *font-families*
                             (make-instance 'freetype-font-family :port port :name name)))

(defclass freetype-face (clim-extensions:font-face)
  ((face :initarg :face
         :reader freetype-face/face)
   (family :type freetype-font-family)))

(defmethod initialize-instance :after ((obj freetype-face) &key port)
  (let* ((face (freetype-face/face obj))
         (family-name (freetype2-types:ft-face-family-name face))
         (name (freetype2-types:ft-face-style-name face)))
    (setf (slot-value obj 'clim-extensions:font-face-family) (find-font-family port family-name))
    (setf (slot-value obj 'clim-extensions:font-face-name) name)))

(defclass freetype-font ()
  ((face :initarg :face
         :reader freetype-font/face)
   (size :initarg :size
         :reader freetype-font/size)
   (lock :initform (bordeaux-threads:make-recursive-lock)
         :reader freetype-font/lock)))

(defmethod print-object ((obj freetype-font) stream)
  (print-unreadable-object (obj stream :type t :identity nil)
    (format stream "FACE ~s SIZE ~s" (freetype-font/face obj) (freetype-font/size obj))))

(defmacro with-size-face ((sym face size) &body body)
  (alexandria:once-only (face size)
    `(progn
       (freetype2:set-char-size ,face (* ,size 64) 0 72 72)
       (let ((,sym ,face))
         ,@body))))

(defmacro with-face-from-font ((sym font) &body body)
  (alexandria:once-only (font)
    `(bordeaux-threads:with-recursive-lock-held ((freetype-font/lock ,font))
       (with-size-face (,sym (freetype-font/face ,font) (freetype-font/size ,font))
         ,@body))))

(defmethod clim-extensions:font-face-all-sizes ((face freetype-face))
  '(8 12 18 20 24 36))

(defmethod clim-extensions:font-face-text-style ((face freetype-face) &optional size)
  (log:info "Making text style: face=~s, size=~s" face size)
  (clim:make-text-style (clim-extensions:font-face-family face) face size))

(defun gcontext-picture (drawable gcontext)
  (flet ((update-foreground (picture)
           ;; FIXME! This makes assumptions about pixel format, and breaks 
           ;; on e.g. 16 bpp displays.
           ;; It would be better to store xrender-friendly color values in
           ;; medium-gcontext, at the same time we set the gcontext 
           ;; foreground. That way we don't need to know the pixel format.
           (let ((fg (the xlib:card32 (xlib:gcontext-foreground gcontext))))
             (xlib::render-fill-rectangle picture
                                          :src                                          
                                          (list (ash (ldb (byte 8 16) fg) 8)
                                                (ash (ldb (byte 8 8) fg) 8)
                                                (ash (ldb (byte 8 0) fg) 8)
                                                #xFFFF)
                                          0 0 1 1))))
    (let* ((fg (xlib:gcontext-foreground gcontext))
           (picture-info
            (or (getf (xlib:gcontext-plist gcontext) 'picture)
                (setf (getf (xlib:gcontext-plist gcontext) 'picture)
                      (let* ((pixmap (xlib:create-pixmap 
                                      :drawable drawable
                                      :depth (xlib:drawable-depth drawable)
                                      :width 1 :height 1))
                             (picture (xlib::render-create-picture
                                       pixmap
                                       :format (xlib::find-window-picture-format
                                                (xlib:drawable-root drawable))
                                       :repeat :on)))
                        (update-foreground picture)
                        (list fg
                             picture
                             pixmap))))))
      (unless (eql fg (first picture-info))
        (update-foreground (second picture-info))
        (setf (first picture-info) fg))
      (cdr picture-info))))

(defun find-rgba-format (drawable)
  (let* ((formats (xlib::render-query-picture-formats (xlib:drawable-display drawable)))
         (format (find-if (lambda (v)
                            (and (= (car (xlib::picture-format-red-byte v)) 8)
                                 (= (car (xlib::picture-format-green-byte v)) 8)
                                 (= (car (xlib::picture-format-blue-byte v)) 8)
                                 (= (car (xlib::picture-format-alpha-byte v)) 8)))
                          formats)))
    (unless format
      (error "Can't find 8-bit RGBA format"))
    format))

(defun create-glyphset (drawable)
  (let ((format (find-rgba-format drawable)))
    (xlib::render-create-glyph-set format)))

(defun render-char-to-glyphset (glyphset face ch)
  (freetype2:load-char face ch '(:force-autohint))
  (let* ((glyph (freetype2-types:ft-face-glyph face))
         (advance (freetype2-types:ft-glyphslot-advance glyph))
         (bitmap (freetype2-types:ft-glyphslot-bitmap (freetype2:render-glyph glyph :lcd))))
    (multiple-value-bind (data type)
        (freetype2:bitmap-to-array bitmap)
      (declare (ignore type))
      #+nil
      (log:info "char=~s, type=~s, dimension=~s, adv-x=~s, adv-y=~s"
                ch type (array-dimensions data)
                (freetype2-types:ft-vector-x advance)
                (freetype2-types:ft-vector-y advance))
      (xlib:render-add-glyph glyphset (char-code ch)
                              :x-origin (freetype2-types:ft-glyphslot-bitmap-left glyph)
                              :y-origin (freetype2-types:ft-glyphslot-bitmap-top glyph)
                              :x-advance (/ (freetype2-types:ft-vector-x advance) 64)
                              :y-advance 0 ; (/ (freetype2-types:ft-vector-x advance) 64)
                              :data (let* ((length (reduce #'* (array-dimensions data)))
                                           (a (make-array (array-dimensions data)
                                                          :element-type '(unsigned-byte 8))))
                                      (loop
                                        for i from 0 below length
                                        do (setf (row-major-aref a i) (row-major-aref data i)))
                                      a)))))

(defun create-dest-picture (drawable)
  (xlib:render-create-picture drawable
                              :format (xlib::find-window-picture-format (xlib:drawable-root drawable))
                              :poly-edge :smooth
                              :poly-mode :precise))

(defun create-pen (drawable)
  (let* ((pixmap (xlib:create-pixmap :drawable (xlib:drawable-root drawable) :width 1 :height 1 :depth 32))
         (picture (xlib:render-create-picture pixmap :format (find-rgba-format drawable) :repeat :on))
         (colour '(0 0 0 #xFFFF)))
    (xlib:render-fill-rectangle picture :over colour 0 0 1 1)
    (xlib:free-pixmap pixmap)
    picture))

(defmethod clim-clx::font-draw-glyphs ((font freetype-font) mirror gc x y string &key start end translate size)
  (log:info "font=~s, mirror=~s, gc=~s, x=~s, y=~s, string=~s, start=~s, end=~s, translate=~s, size=~s"
            font mirror gc x y string start end translate size)
  (let ((glyphset (create-glyphset mirror)))
    ;; hard-code loading of the ASCII glyphset, for testing purposes
    (with-face-from-font (face font)
      (loop
        for i from 33 below 128
        do (render-char-to-glyphset glyphset face (code-char i))))
    (let ((source (create-pen mirror))
          (dest (create-dest-picture mirror)))
      (xlib:render-composite-glyphs dest glyphset source x y
                                    (map 'vector #'char-code "Foo")
                                    :start start :end end)
      (xlib:render-free-picture source)
      (xlib:render-free-picture dest)
      (xlib:render-free-glyph-set glyphset))))

(defmethod clim-clx::font-text-extents ((font freetype-font) string &key (start 0) (end (length string)) translate)
  (declare (ignore translate))
  (log:info "Getting text extents for ~s, with string: ~s" font string)
  ;; Values to return:
  ;;   width ascent descent left right font-ascent font-descent direction first-not-done
  (with-face-from-font (face font)
    (let* ((s (subseq string start end))
           (width (freetype2:string-pixel-width face s)))
      (values width
              (freetype2:face-ascender-pixels face)
              (freetype2:face-descender-pixels face)
              0
              width
              (freetype2:face-ascender-pixels face)
              (freetype2:face-descender-pixels face)
              0
              end))))

(defmethod clim-clx::font-ascent ((font freetype-font))
  (with-face-from-font (face font)
    (freetype2:face-ascender-pixels face)))

(defmethod clim-clx::font-descent ((font freetype-font))
  (with-face-from-font (face font)
    (freetype2:face-descender-pixels face)))

(defmethod clim-clx::text-style-to-x-font ((port clx-freetype-port) (text-style climi::device-font-text-style))
  (log:debug "finding font for device-font-text-style: ~s" text-style))

(defun find-freetype-font (text-style)
  (multiple-value-bind (family face size)
      (clim:text-style-components text-style)
    (declare (ignore family face))
    (make-instance 'freetype-font
                   :face *face*
                   :size (etypecase size
                           (keyword (or (getf clim-clx::*clx-text-sizes* size) 12))
                           (number size)))))

(defmethod clim-clx::text-style-to-x-font ((port clx-freetype-port) (text-style clim:standard-text-style))
  (log:debug "finding font for standard-text-style: ~s" text-style)
  (or (clim:text-style-mapping port text-style)
      (setf (climi::text-style-mapping port text-style)
            (find-freetype-font text-style))))

;;;
;;;  Character info
;;;

(defmethod clim-clx::font-glyph-width ((font freetype-font) char)
  (log:info "getting width of ~s" char))

(defmethod clim-clx::font-glyph-left ((font freetype-font) char)
  (with-face-from-font (face font)
    (freetype2:load-char face char)
    (let* ((glyph (freetype2-types:ft-face-glyph *face*))
           (metrics (freetype2-types:ft-glyphslot-metrics glyph)))
      (/ (freetype2-types:ft-glyph-metrics-hori-bearing-x metrics) *freetype-font-scale*))))

(defmethod clim-clx::font-glyph-right ((font freetype-font) char)
  (with-face-from-font (face font)
    (freetype2:load-char face char)
    (let* ((glyph (freetype2-types:ft-face-glyph *face*))
           (metrics (freetype2-types:ft-glyphslot-metrics glyph)))
      (/ (- (freetype2-types:ft-glyph-metrics-width metrics)
            (freetype2-types:ft-glyph-metrics-hori-advance metrics))
         *freetype-font-scale*))))