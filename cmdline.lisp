(defpackage :cmdline-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :cmdline-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (progn
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3))
  (unless (find-package "CLIM")
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type foo-argument ()
    :inherit-from t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo-argument ()
    ((content :initarg :content
              :reader foo-argument/content))))

(defmethod print-object ((obj foo-argument) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~s" (foo-argument/content obj))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type foo-argument-or-command
      (&key (command-table (clim:frame-command-table clim:*application-frame*)))))

(clim:define-command-table maxima-commands)

(defclass maxima-interactor-view (clim:textual-view)
  ())

(defparameter +listener-view+ (make-instance 'maxima-interactor-view))

(defclass maxima-interactor-pane (clim:interactor-pane)
  ())

(clim:define-presentation-type maxima-empty-input ())

(clim:define-application-frame maxima-main-frame ()
  ()
  (:panes (text-content (clim:make-clim-stream-pane :type 'maxima-interactor-pane
                                                    :name 'maxima-interactor
                                                    :default-view +listener-view+)))
  (:top-level (clim:default-frame-top-level :prompt 'print-listener-prompt))
  (:command-table (maxima-main-frame :inherit-from (maxima-commands)))
  (:layouts (default (clim:vertically ()
                       text-content))))

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

(defun read-plain-text (stream
                        &key
                          (input-wait-handler clim:*input-wait-handler*)
		          (pointer-button-press-handler clim:*pointer-button-press-handler*)
		          click-only)
  (declare (ignore click-only))
  (let ((result (make-array 1 :adjustable t :fill-pointer 0 :element-type 'character)))
    (loop
      for first-char = t then nil
      for gesture = (clim:read-gesture :stream stream
		                       :input-wait-handler input-wait-handler
		                       :pointer-button-press-handler pointer-button-press-handler)
      do (cond ((or (null gesture)
		    (clim:activation-gesture-p gesture)
		    (typep gesture 'clim:pointer-button-event)
		    (clim:delimiter-gesture-p gesture))
		(loop-finish))
	       ((characterp gesture)
		(vector-push-extend gesture result))
	       (t nil))
      finally (progn
		(when gesture
		  (clim:unread-gesture gesture :stream stream))
		(return (subseq result 0))))))

(clim:define-presentation-method clim:accept ((type foo-argument)
                                              stream (view clim:textual-view)
                                              &key
                                              (default nil defaultp)
                                              (default-type type))
  (let ((s (read-plain-text stream)))
    (log:trace "Got string from reading native expr: ~s" s)
    (let ((trimmed (string-trim " " s)))
      (log:info "trimmed = ~s" trimmed)
      (if (equal trimmed "")
          (if defaultp
              (values default default-type)
              (values nil 'maxima-empty-input))
          (values (make-instance 'foo-argument :content trimmed) type)))))

(clim:define-presentation-method clim:present (obj (type foo-argument) stream (view t) &key)
  (clim:with-room-for-graphics (*standard-output* :first-quadrant nil)
    (clim:surrounding-output-with-border (*standard-output*)
      (clim:draw-text* *standard-output* (if obj (foo-argument/content obj) "nil") 0 0 :ink clim:+green+))))

(clim:define-presentation-method clim:present (obj (type foo-argument) (stream string-stream) view &key)
  (format stream "~a" (foo-argument/content obj)))

(clim:define-presentation-method clim:accept ((type foo-argument-or-command)
                                              stream
                                              (view clim:textual-view)
                                              &key)
  (let ((command-ptype `(clim:command :command-table ,command-table)))
    (clim:with-input-context (`(or ,command-ptype foo-argument))
        (object type event options)
        (let ((initial-char (clim:read-gesture :stream stream :peek-p t)))
	  (if (member initial-char clim:*command-dispatchers*)
	      (progn
		(clim:read-gesture :stream stream)
		(clim:accept command-ptype :stream stream :view view :prompt nil :history 'clim:command))
	      (clim:accept 'foo-argument :stream stream :view view :prompt nil :history 'foo-argument-or-command)))
      (t
       (funcall (cdar clim:*input-context*) object type event options)))))

(clim:define-command (maxima-eval :name "Run command" :menu t :command-table maxima-commands)
    ((argument 'foo-argument :prompt "command"))
  (format t "Argument:")
  (present-to-stream argument *standard-output*))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\:)))
        (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
          (clim:accept 'foo-argument-or-command :stream stream :prompt nil
                                                :default nil :default-type 'maxima-empty-input
                                                :history 'foo-argument-or-command)))
    (log:info "Got input: object=~s, type=~s" object type)
    (when object
      `(maxima-eval ,object))))
