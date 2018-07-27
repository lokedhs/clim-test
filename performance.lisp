(defpackage :perf-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :perf-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim")))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(clim:define-application-frame main-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content))
  (:layouts (default (clim:vertically ()
                       text-content))))


(defun display-text-content (frame stream)
  (declare (ignore frame))
  (let ((rec (clim:with-output-to-output-record (stream)
               (loop
                 for x from 0 to 200
                 do (format stream "Line with number ~s: Some text ~r. Test foo test. Test foo test. Test foo test.~%" x x)))))
    (clim:with-room-for-graphics (stream)
      (clim:stream-add-output-record stream rec)))
  (format stream "hello~%"))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))
