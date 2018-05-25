(defpackage :command-presentation
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :command-presentation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo ()
    ((value :type string
            :initarg :value
            :initform "foo"
            :reader foo/value)))

  (defclass foo-view (clim:textual-view)
    ()))

(defvar +foo-view+ (make-instance 'foo-view))

(clim:define-command-table foo-commands)

(defun display-text-content (frame stream)
  (format stream "Some content~%")
  (dolist (obj (foo-frame/objects frame))
    (present-to-stream obj stream)))

(clim:define-application-frame foo-frame ()
  ((values :initform nil
            :accessor foo-frame/objects))
  (:panes (text-content :application
                        :display-function 'display-text-content
                        :default-view +foo-view+)
          (interaction-pane :interactor))
  (:command-table (foo-frame :inherit-from (foo-commands)))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 600
                                            :height 600)))
    (clim:run-frame-top-level frame)))

(clim:define-presentation-translator foo-to-string (string foo foo-commands)
    (object)
  (log:info "Calling command translator for: ~s" object)
  (make-instance 'foo :value object))

(clim:define-presentation-translator string-to-foo (foo string foo-commands)
    (object)
  (log:info "Transforming foo to string: ~s" object)
  (foo/value object))

(clim:define-presentation-method clim:present (obj (type foo) stream (view foo-view) &key)
  (let ((value (foo/value obj)))
    (clim:with-room-for-graphics (stream :first-quadrant nil)
      (clim:draw-text* stream value 0 0)
      (clim:draw-line* stream 0 0 50 0))))

(clim:define-presentation-method clim:present (obj (type foo) stream (view clim:textual-view) &key)
  (format stream "~a" (foo/value obj)))

(clim:define-command (show-object-command :name "Show object" :menu t :command-table foo-commands)
    ((obj 'foo :prompt "Object")
     (text 'string :prompt "Text"))
  (format *standard-output* "Text is: ~a~%" text)
  (push obj (foo-frame/objects clim:*application-frame*)))

(clim:define-command (show-string-object-command :name "Show string object" :menu t :command-table foo-commands)
    ((value 'string :prompt "Value"))
  (push (make-instance 'foo :value value) (foo-frame/objects clim:*application-frame*)))
