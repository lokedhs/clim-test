(defpackage :cmdline-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :cmdline-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(clim:define-command-table maxima-commands)

(defclass maxima-interactor-view (clim:textual-view)
  ())

(defparameter +listener-view+ (make-instance 'maxima-interactor-view))

(defclass maxima-interactor-pane (clim:interactor-pane)
  ())

(clim:define-application-frame maxima-main-frame ()
  ()
  (:panes (text-content (clim:make-clim-stream-pane :type 'maxima-interactor-pane
                                                    :name 'maxima-interactor
                                                    :default-view +listener-view+)))
  (:top-level (clim:default-frame-top-level :prompt 'print-listener-prompt))
  (:command-table (maxima-main-frame :inherit-from (maxima-commands)))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\;)))
        (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
          (clim:accept 'string :stream stream :prompt nil :default "" :default-type 'string)))
    (log:info "Got input: object=~s, type=~s" object type)
    (let ((expression (string-trim " " object)))
      (unless (equal expression "")
        `(maxima-eval ,expression)))))

(defmethod clim:stream-present :around ((stream maxima-interactor-pane) object type
                                   &rest args
                                   &key (single-box nil single-box-p) &allow-other-keys)
  (declare (ignore single-box single-box-p))
  (apply #'call-next-method stream object type :single-box t args))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (format stream "maxima> "))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'maxima-main-frame)))
    (clim:run-frame-top-level frame)))

(clim:define-command (maxima-eval :name "Run command" :menu t :command-table maxima-commands)
    ((cmd 'string :prompt "command"))
  (log:info "eval command. form=~s" cmd)
  #+nil
  (let ((rec (clim:with-output-to-output-record (*standard-output*)
               (clim:with-text-style (*standard-output* (clim:make-text-style "DejaVu Sans" "Book" 12))
                 (clim:draw-line* *standard-output* 0 0 100 50)
                 (clim:draw-text* *standard-output* "Foo" 100 10)))))
    (clim:with-room-for-graphics (*standard-output*)
      (clim:stream-add-output-record *standard-output* rec)))
  (clim:with-room-for-graphics (*standard-output*)
    (clim:with-text-style (*standard-output* (clim:make-text-style "DejaVu Sans" "Book" 12))
      (clim:draw-line* *standard-output* 0 0 100 50)
      (clim:draw-text* *standard-output* "Foo" 100 10))))

(defun maxima::$plotclim (&rest foo)
  (log:info "Got params: ~s" foo))
