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

(defun draw-x (stream x y)
  (clim:draw-line* stream (- x 5) (- y 5) (+ x 5) (+ y 5) :ink clim:+blue+)
  (clim:draw-line* stream (- x 5) (+ y 5) (+ x 5) (- y 5) :ink clim:+blue+))

(defun xdisplay-text-content (frame stream)
  (declare (ignore frame))
  (draw-x stream 50 100)
  (clim:draw-text* stream "Foo" 50 100 :transformation (clim:make-rotation-transformation* 0.3 50 100)))

(defun display-text-content (frame stream)
  (declare (ignore frame)) 
  (loop
    for i from 0 below 200
    do (format stream "This is line ~d. Lots of text ~r. Even more text here to fill the entire window.~%" i i)))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))
