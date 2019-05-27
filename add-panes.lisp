(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (ql:quickload "log4cl")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim")))

(defpackage :clim-test.add-panes
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.add-panes)

(defun display-inner (frame pane)
  (declare (ignore frame))
  (clim:draw-line* pane 10 10 100 600))

(defun make-workbench ()
  (let ((inner (clim:make-clim-stream-pane :display-function 'display-inner)))
    (clim:make-pane 'clim:vrack-pane :name 'workbench-pane :contents (list inner))))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (workbench-pane (make-workbench)))
  (:layouts (default (clim:vertically ()
                       workbench-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
