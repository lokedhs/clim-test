(defpackage :maths-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :maths-test)

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defclass text-content-view (clim:view)
  ())

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "This is some test content~%")
  (math-interactor:put-result (polynomials:make-polynomial 10 2 4 8 8)))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
