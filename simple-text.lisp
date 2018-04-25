(defpackage :simple-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :simple-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(defun draw-rotated-rectangle (stream x y colour)
  (clim:with-drawing-options (stream :transformation (clim:make-rotation-transformation* 0.5))
    (clim:draw-rectangle* stream x y (+ x 100) (+ y 50) :ink colour :filled nil)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (draw-rotated-rectangle stream 10 10 clim:+green+)
  (let ((rec (clim:with-output-to-output-record (stream)
               (draw-rotated-rectangle stream 50 50 clim:+blue+))))
    (clim:stream-add-output-record stream rec)))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content
                        :redisplay-on-resize-p t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
