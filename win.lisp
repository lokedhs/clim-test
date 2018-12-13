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

(clim:define-presentation-type foo-type ())

(clim:define-presentation-method clim:present (obj (type foo-type) stream view &key)
  (format stream "Foo-type: ~a" obj))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (documentation-content :application
                                 :display-function 'display-content
                                 :display-time t
                                 :incremental-redisplay t)
          (main-input (clim:make-pane 'clim:text-field-pane :name 'main-input))
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       documentation-content
                       main-input
                       interaction-pane))))

(defun display-content (frame stream)
  (declare (ignore frame))
  (format stream "Test content~%")
  (clim:stream-present stream "main-frame" 'foo-type))

(defun display-popup-content (frame stream)
  (declare (ignore frame))
  (format stream "This is the content of the popup~%")
  (clim:stream-present stream "popup-frame" 'foo-type))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))

(clim:define-application-frame maxima-documentation-frame ()
  ()
  (:panes (popup-content :application
                         :display-function 'display-popup-content)
          (doc-input (clim:make-pane 'clim:text-field-pane :name 'doc-input)))
  (:layouts (default (clim:vertically ()
                       doc-input
                       popup-content))))

(define-foo-frame-command (open-window-command :name "Open Window")
    ()
  (let* ((fm (clim:frame-manager clim:*application-frame*))
         (frame (clim:make-application-frame 'maxima-documentation-frame
                                             :frame-manager fm
                                             :width 400
                                             :height 400
                                             :event-queue (climi::frame-event-queue clim:*application-frame*))))
    #+nil (clim:adopt-frame fm frame)
    (log:info "q = ~s" (climi::frame-event-queue clim:*application-frame*))
    #+nil (push frame (slot-value fm 'climi::frames))
    (let ((toplevel (clim:frame-top-level-sheet frame))
          (graft (clim:find-graft :port (clim:port fm))))
      (assert (not (null toplevel)))
      (setf (slot-value toplevel 'climi::frame) clim:*application-frame*)
      (clim:sheet-adopt-child graft toplevel)
      #+nil (setf (clim:graft frame) graft))
    (clim:enable-frame frame)
    (let ((pane (clim:find-pane-named clim:*application-frame* 'documentation-content)))
      (log:info "pane = ~s" pane)
      #+nil (assert (not (null pane)))
      ;; This is just a test loop to see if the window updates at all.
      ;; Currently, it does not. But the error doesn't seem to be
      ;; directly related to the event loop. Instead, it seems to crash
      ;; because there is something wrong with the setup of the panes
      ;; in the frame.
      #+nil
      (loop
        named control-loop
        for gesture = (clim:read-gesture :stream pane)
        do (log:info "gesture = ~s" gesture)))))

(define-foo-frame-command (show-foo-type :name "Show")
    ((obj foo-type :prompt "Select instance"))
  (log:info "Selected objected: ~s" obj))

(define-foo-frame-command (toplevel-application-pane-command :name "Toplevel")
    ()
  (let ((frame (clim:make-application-frame 'maxima-documentation-frame
                                            :width 400
                                            :height 400)))
    (bordeaux-threads:make-thread (lambda ()
                                    (clim:run-frame-top-level frame)))))
