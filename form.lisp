(defpackage :form
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :form)

(clim:define-application-frame main-frame ()
  ()
  (:panes (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       interaction-pane))))

(define-main-frame-command (com-input :name "Input" :menu t)
  ((arg1 string)
   (arg2 string)
   (arg3 string))
  (format t "arg1=~s arg2=~s arg3=~s~%" arg1 arg2 arg3))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame)))
    (clim:run-frame-top-level frame)))
