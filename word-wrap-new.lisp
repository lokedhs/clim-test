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

(clim:define-presentation-method clim:present (obj (type foo) stream view &key)
  (clim:with-room-for-graphics (stream :first-quadrant nil :move-cursor t)
    (clim:draw-rectangle* stream 0 0 10 20 :ink clim:+blue+)
    (clim:draw-rectangle* stream 0 0 10 -10 :ink clim:+green+)
    (clim:draw-text* stream "Foo" 15 0)))

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
  (format stream "This is the first line.~%")
  (clim:indenting-output (stream 40)
    (loop
      for i from 0 below 10
      when (= i 2)
        do (clim:stream-present stream (make-instance 'foo) 'foo)
      do (format stream "This is line ~d. Lots of text ~r. Even more text here to fill the entire window. " i i))))

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
