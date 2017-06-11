(defpackage :clim-test.incremental
  (:use :cl)
  (:export #:open-resize-test-frame))

(in-package :clim-test.incremental)

(clim:define-application-frame resize-test-frame ()
  ((rows :initform 0
         :accessor resize-test-frame/rows))
  (:panes (content :application
                   :display-function 'display-content
                   :display-time t
                   :incremental-redisplay t)
          (interaction-pane :interactor))
  (:layouts (default content
                     interaction-pane)))

(defun display-content (frame stream)
  (loop
    for i from 0 below (resize-test-frame/rows frame)
    do (format stream "Row number: ~a~%" i)))

(defun open-resize-test-frame ()
  (let ((frame (clim:make-application-frame 'resize-test-frame)))
    (clim:run-frame-top-level frame)))

(define-resize-test-frame-command (rows :name "Rows")
    ((n 'integer))
  (let ((frame clim:*application-frame*))
    (setf (resize-test-frame/rows frame) n)
    (setf (clim:pane-needs-redisplay (clim:find-pane-named frame 'content)) t)))
