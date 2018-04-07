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
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (unless *face*
      (setq *face* (freetype2:new-face #p"/home/elias/.fonts/NotoSans-Regular.ttf"))
      (freetype2:set-char-size *face* (* 18 64) 0 72 72))
    (clim:run-frame-top-level frame)))

(defvar *font-families* (make-hash-table :test 'equal))

(defclass freetype-font-family (clim-extensions:font-family)
  ((name :type string
         :initarg name)))

(defun find-font-family (name)
  (alexandria:ensure-gethash name *font-families* (make-instance 'freetype-font-family :name name)))

(defclass freetype-face (clim-extensions:font-face)
  ((face :initarg :face)
   (family :type freetype-font-family)))

(defmethod initialize-instance :after ((obj freetype-face) &key)
  (setf (slot-value obj 'family) (find-font-family (clim-extensions:font-face-name obj))))

(defclass freetype-font ()
  ())

(defmethod clim-extensions:font-face-all-sizes ((face freetype-face))
  '(8 12 18 20 24 36))

(defmethod clim-extensions:font-face-text-style ((face freetype-face) &optional size)
  (log:info "Making text style: face=~s, size=~s" face size)
  (clim:make-text-style (clim-extensions:font-face-family face) face size))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (clim:draw-text* stream "Foo" 40 40 :text-face (make-instance 'freetype-face :face *face*)))

(defmethod clim-clx::font-draw-glyphs ((font freetype-face) mirror gc x y string &key start end translate size)
  (log:info "font=~s, mirror=~s, gc=~s, x=~s, y=~s, string=~s, start=~s, end=~s, translate=~s, size=~s"
            font mirror gc x y string start end translate size)
  #+nil
  (freetype2:do-string-render (*face* "Foo" bitmap x y)
    (log:info "Rendering bitmap: ~s" bitmap)))
