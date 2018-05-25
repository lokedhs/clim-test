(defpackage :simple-text
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :simple-text)

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

(defun display-text-content (frame stream)
  (declare (ignore frame)) 
  (loop
    for i from 0 below 200
    do (format stream "Line number ~d: Some more text here: ~r. And add even more text here to make drawing slower.~%" i i)))

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
