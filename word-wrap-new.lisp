(defpackage :clim-test.wrap
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.wrap)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo ()
    ()))

(defun draw-x (stream x y)
  (clim:draw-line* stream (- x 5) (- y 5) (+ x 5) (+ y 5) :ink clim:+blue+)
  (clim:draw-line* stream (- x 5) (+ y 5) (+ x 5) (- y 5) :ink clim:+blue+))

(defun call-with-stream-content (stream fn)
  (let ((rec (clim:with-output-to-output-record (stream)
               (funcall fn stream))))
    (clim:with-bounding-rectangle* (x1 y1 x2 y2)
        rec
      (declare (ignore y1 y2))
      (let ((rec-baseline (clime:output-record-baseline rec)))
        (multiple-value-bind (cursor-x cursor-y)
            (clim:stream-cursor-position stream)
          (clim:stream-add-output-record stream rec)
          (setf (clim:output-record-position rec) (values cursor-x (+ (- cursor-y rec-baseline) (clim:stream-baseline stream))))
          (setf (clim:stream-cursor-position stream) (values (+ cursor-x (- x2 x1)) cursor-y))
          (draw-x stream cursor-x cursor-y))))))

(defmacro with-stream-content ((stream) &body body)
  (check-type stream symbol)
  `(call-with-stream-content ,stream (lambda (,stream) ,@body)))

(defclass zero-y-baseline-output-record (clim:standard-sequence-output-record)
  ()
  (:documentation "Output record type that defines the baseline to be at y position 0"))

(defmethod clime:output-record-baseline ((rec zero-y-baseline-output-record))
  (multiple-value-bind (x y)
      (clim:output-record-position rec)
    (declare (ignore x))
    (log:info "returning ~s" (- y))
    (values (- y) t)))

(clim:define-presentation-method clim:present (obj (type foo) stream view &key)
  (let ((rec (clim:with-output-to-output-record (stream 'zero-y-baseline-output-record)
               (clim:draw-rectangle* stream 0 0 10 10 :ink clim:+blue+)
               (clim:draw-rectangle* stream 0 0 10 -30 :ink clim:+green+)
               (clim:draw-text* stream "Foo" 15 0))))
    (clim:stream-add-output-record stream rec)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "This is the first line.~%")
  (format stream "Abc ")
  (with-stream-content (stream)
    (clim:stream-present stream (make-instance 'foo) 'foo))
  (format stream "Def ")
  (clim:with-text-size (stream 30)
    (format stream "Ghi")))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content
                        :end-of-line-action :wrap*
                        :end-of-page-action :allow))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))
