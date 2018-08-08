(defpackage :interactor-focus-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :interactor-focus-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim")))

(clim:define-application-frame main-frame ()
  ()
  (:panes (editor (clim:make-pane 'drei:drei-pane))
          (interactor :interactor))
  (:layouts (default (clim:vertically ()
                       editor
                       interactor))))


(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(define-main-frame-command (focus-editor :name "Focus editor")
    ()
  (let ((editor (clim:find-pane-named clim:*application-frame* 'editor)))
    (clim:stream-set-input-focus editor)))
