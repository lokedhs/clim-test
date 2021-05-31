(defpackage :menu-test2
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :menu-test2)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(clim:define-command-table expression-commands)

(clim:define-command-table foo-commands
  :inherit-from (expression-commands))

(defun draw-test-rectangle (stream x y colour)
  (clim:draw-rectangle* stream x y (+ x 100) (+ y 50) :ink colour :filled nil))

(defun draw-x (stream x y)
  (clim:draw-line* stream (- x 5) (- y 5) (+ x 5) (+ y 5) :ink clim:+blue+)
  (clim:draw-line* stream (- x 5) (+ y 5) (+ x 5) (- y 5) :ink clim:+blue+))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (loop
    for i from 0 below 10
    do (format stream "This is line ~d. Lots of text ~r. Even more text here to fill the entire window.~%" i i)))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content)
          #+nil (interaction-pane :interactor))
  (:menu-bar foo-menubar-command-table)
  (:command-table (foo-main-frame :inherit-from (foo-commands)))
  (:layouts (default (clim:vertically ()
                       text-content
                       #+nil interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(clim:define-presentation-type foo-expression-or-command
    (&key (command-table (clim:frame-command-table clim:*application-frame*)))
  :inherit-from t)

(defmethod clim:read-frame-command ((frame foo-frame) &key (stream *standard-input*))
  (let ((result (clim:accept 'foo-expression-or-command :stream stream)))
    (log:info "result = ~s" result)))

(clim:define-presentation-method clim:accept ((type foo-expression-or-command)
                                              stream
                                              view
                                              &key)
  (let ((command-ptype `(clim:command :command-table ,command-table)))
    (clim:with-input-context (command-ptype)
        (object type event options)
        (clim:accept 'string :stream stream)
      (t
       (log:trace "other command type: obj=~s type=~s ev=~s options=~s" object type event options)
       (funcall (cdar clim:*input-context*) object type event options)))))

(clim:define-command (foo-quit :name "Quit" :menu t :command-table foo-commands)
    ()
  (log:info "Quit selected"))

(clim:make-command-table 'foo-menubar-command-table
                         :errorp nil
                         :menu '(("File" :menu foo-file-command-table)))

(clim:make-command-table 'foo-file-command-table
                         :errorp nil
                         :menu '(("Quit" :command foo-quit)))
