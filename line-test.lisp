(defpackage :line-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :line-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (unless (find-package "LOG4CL")
      (ql:quickload "log4cl"))
    (ql:quickload "mcclim")))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defclass text-content-view (clim:view)
  ())

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame :width 700 :height 800)))
    (clim:run-frame-top-level frame)))

(defun draw-x (stream x y &optional (size 5))
  (clim:draw-line* stream (- x size) (- y size) (+ x size) (+ y size))
  (clim:draw-line* stream (- x size) (+ y size) (+ x size) (- y size)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (loop
    for i from 1 to 20
    do (progn
         (clim:draw-line* stream
                          (+ 10 (* i 30)) 10
                          (+ 10 (* i 30)) 14
                          :line-thickness 1)
         (clim:draw-line* stream
                          (+ 10 (* i 30)) 15
                          (+ 10 (* i 30)) 50
                          :line-thickness i)
         (clim:draw-line* stream
                          (+ 10 (* i 30)) 70
                          (+ 50 (* i 30)) 120
                          :line-thickness i)
         (clim:draw-line* stream
                          10 (+ (* i 30) 150)
                          390 (+ (* i 30) 150)
                          :line-thickness i)
         (clim:draw-line* stream
                          392 (+ (* i 30) 150)
                          400 (+ (* i 30) 150)
                          :line-thickness 1)
         (clim:draw-text* stream (format nil "~2d" i) 420 (+ (* i 30) 150)))))
