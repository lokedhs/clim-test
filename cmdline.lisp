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

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defclass foo-argument ()
  ((content :initarg :content
            :reader foo-argument/content)))

(defmethod print-object ((obj foo-argument) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (foo-argument/content obj))))

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
          (clim:accept 'foo-argument :stream stream :prompt nil :default nil :default-type 'foo-argument)))
    (log:info "Got input: object=~s, type=~s" object type)
    (when object
      `(maxima-eval ,object))))

(defmethod clim:stream-present :around ((stream maxima-interactor-pane) object type
                                   &rest args
                                   &key (single-box nil single-box-p) &allow-other-keys)
  (declare (ignore single-box single-box-p))
  (apply #'call-next-method stream object type :single-box t args))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (format stream "foo> "))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'maxima-main-frame)))
    (clim:run-frame-top-level frame)))

(clim:define-presentation-method clim:accept ((type foo-argument) stream (view clim:textual-view) &key)
  (let ((s (clim:accept 'string :stream stream :view view :prompt nil :history 'foo-argument)))
    (make-instance 'foo-argument :content s)))

(clim:define-presentation-method clim:present (obj (type foo-argument) stream (view t) &key)
  (clim:with-room-for-graphics (*standard-output* :first-quadrant nil)
    (clim:surrounding-output-with-border (*standard-output*)
      (clim:draw-text* *standard-output* (if obj (foo-argument/content obj) "nil") 0 0 :ink clim:+green+))))

(clim:define-command (maxima-eval :name "Run command" :menu t :command-table maxima-commands)
    ((argument 'foo-argument :prompt "command"))
  (format t "Argument:")
  (present-to-stream argument *standard-output*))
