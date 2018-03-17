(defpackage :maths-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :maths-test)

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defclass text-content-view (clim:textual-view)
  ())

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "This is some test content~%")
  (let ((expression (polynomials:make-polynomial 10 2 4))
        (output-record nil))
    (clim:with-output-recording-options (stream :draw nil)
      (setf output-record (rtc:render (math-utils-format:format-pretty expression :presentations t)
                                      stream))
      (rtc:advance-cursor output-record stream :line-break t))))

(clim:define-application-frame foo-frame ()
  ((math-interactor::formatted-content :initform (list (list 'text-content))))
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
