(defpackage :simple-text
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :simple-text)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(defun draw-test-rectangle (stream x y colour)
  (clim:draw-rectangle* stream x y (+ x 100) (+ y 50) :ink colour :filled nil))

(defun display-text-content (frame stream)
  (declare (ignore frame)) 
  (draw-test-rectangle stream 10 10 clim:+green+)
  (let ((rec (clim:with-output-to-output-record (stream)
               (draw-test-rectangle stream 50 50 clim:+blue+))))
    (clim:stream-add-output-record stream rec)))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
