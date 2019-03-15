(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (ql:quickload "log4cl")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim")))

(defpackage :clim-test.template
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.template)

(clim:define-application-frame foo-frame ()
  ()
  (:panes (content :application
                   :display-function 'display-content
                   :display-time t
                   :incremental-redisplay t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       content
                       interaction-pane)))
  (:geometry :width 600 :height 600))

(defun display-content (frame stream)
  (declare (ignore frame))
  (format stream "Test content~%"))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
