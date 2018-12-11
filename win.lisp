(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (ql:quickload "log4cl")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim")))

(defpackage :clim-test.window
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.window)

(clim:define-application-frame foo-frame ()
  ()
  (:panes (documentation-content :application
                                 :display-function 'display-content
                                 :display-time t
                                 :incremental-redisplay t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       documentation-content
                       interaction-pane))))

(defun display-content (frame stream)
  (declare (ignore frame))
  (format stream "Test content"))

(defun display-popup-content (frame stream)
  (declare (ignore frame))
  (format stream "This is the content of the popup"))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))

(clim:define-application-frame maxima-documentation-frame ()
  ()
  (:panes (content :application
                   :display-function 'display-popup-content))
  (:layouts (default (clim:vertically ()
                       content))))

(define-foo-frame-command (open-window-command :name "Open Window")
    ()
  (let* ((fm (clim:frame-manager clim:*application-frame*))
         (frame (clim:make-application-frame 'maxima-documentation-frame
                                             :frame-manager fm
                                             :width 400
                                             :height 400)))
    (clim:adopt-frame fm frame)
    (clim:enable-frame frame)
    (let ((pane (clim:find-pane-named frame 'documentation-content)))
      (assert (not (null pane)))
      ;; This is just a test loop to see if the window updates at all.
      ;; Currently, it does not. But the error doesn't seem to be
      ;; directly related to the event loop. Instead, it seems to crash
      ;; because there is something wrong with the setup of the panes
      ;; in the frame.
      (loop
        named control-loop
        for gesture = (clim:read-gesture :stream pane)
        do (log:info "gesture = ~s" gesture)))))
