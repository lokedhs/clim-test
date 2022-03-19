;;; Demonstration of issue introduced in e3fcfaa22ebe1666fa3b455fcc90b6200b46652e

(defpackage :simple-multi-arg
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :simple-multi-arg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(clim:define-command-table foo-commands)

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "To illustrate the problem,
type the command Foo and
press return. Then attempt
to enter two different
values in the form.

When one attempts to
move to the second field,
the first field gets cleared."))

(clim:define-application-frame foo-frame ()
  ((values :initform nil
            :accessor foo-frame/objects))
  (:panes (text-content :application
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:menu-bar menubar-command-table)
  (:command-table (foo-frame :inherit-from (foo-commands)))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 600
                                            :height 600)))
    (clim:run-frame-top-level frame)))

(clim:define-command (foo-command :name "Foo" :menu t :command-table foo-commands)
    ((arg0 'string :prompt "arg0")
     (arg1 'string :prompt "arg1"))
  (format *debug-io* "arg0 is: ~a~%arg1 is: ~a~%" arg0 arg1))

(clim:define-command (bar-command :name "Bar" :menu t :command-table foo-commands)
    ((arg0 'string :prompt "arg0")
     &key
     (key-arg 'string :prompt "Key arg"))
  (format *debug-io* "arg0=~s key-arg=~s~%" arg0 key-arg))

(clim:make-command-table 'menubar-command-table
                         :errorp nil
                         :menu '(("File" :menu file-command-table)))

(clim:make-command-table 'file-command-table
                         :errorp nil
                         :menu '(("Foo" :command (foo-command))))

(defmethod clim:read-frame-command :around ((frame foo-frame) &key (stream *standard-input*))
  (declare (ignore stream))
  (log:info "Before read frame command")
  (handler-bind ((t (lambda (condition)
                      (log:info "Got condition: ~s" condition))))
    (let ((res (multiple-value-list (call-next-method))))
      (log:info "Result: ~s" res)
      (apply #'values res))))
