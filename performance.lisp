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

(clim:define-presentation-type html-text ())

(clim:define-presentation-method clim-internals::convert-clipboard-content
    (obj (type html-text) (output-type (eql :string)) check-only)
  obj)

(clim:define-presentation-method clim-internals::convert-clipboard-content
    (obj (type html-text) (output-type (eql :html)) check-only)
  obj)

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(clim:define-application-frame main-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content)
          (interactor :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interactor))))


(defun display-text-content (frame stream)
  (declare (ignore frame))
  (let ((rec (clim:with-output-to-output-record (stream)
               (loop
                 for x from 0 below 20
                 do (format stream "Line with number ~s: Some text ~r. Test foo test. Test foo test. Test foo test.~%" x x)))))
    (clim:with-room-for-graphics (stream)
      (clim:stream-add-output-record stream rec)))
  (clim:present "Plain string" 'string :stream stream)
  (format stream "~%")
  (clim:present "Some <b>html text</b>" 'html-text :stream stream))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))
