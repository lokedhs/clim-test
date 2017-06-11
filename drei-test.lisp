(defpackage :clim-test.drei
  (:use :cl)
  (:export #:open-drei-test-frame))

(in-package :clim-test.drei)

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "Some text here~%")
  (clim:with-output-as-gadget (stream)
    (clim:make-pane 'drei:drei-pane :height 50))
  (format stream "~%This text should be below the editor~%"))

(clim:define-application-frame drei-test-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-drei-test-frame ()
  (let ((frame (clim:make-application-frame 'drei-test-frame)))
    (clim:run-frame-top-level frame)))
