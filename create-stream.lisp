(defpackage :create-stream
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :create-stream)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim")))

(clim:define-command-table foo-commands)

(clim:define-application-frame main-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:command-table (main-frame :inherit-from (foo-commands)))
  (:layouts (default (clim:vertically (:name 'vertical-pane)
                       text-content
                       interaction-pane))))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "To reproduce the issue, type :Foo Command in the interactor"))

(clim:define-presentation-type string-or-command (&key (command-table (clim:frame-command-table clim:*application-frame*)))
  :inherit-from t)

(clim:define-presentation-method clim:accept ((type string-or-command)
                                              stream
				              (view clim:textual-view)
				              &key)
  (let ((command-ptype `(clim:command :command-table ,command-table)))
    (clim:with-input-context (command-ptype)
        (object type event options)
        (clim:with-input-context ('string :override nil)
            (inner-object inner-type inner-event inner-options)
            (let ((initial-char (clim:read-gesture :stream stream :peek-p t)))
	      (if (member initial-char clim:*command-dispatchers*)
	          (progn
		    (clim:read-gesture :stream stream)
                    (clim:accept command-ptype :stream stream :view view :prompt nil :history nil))
	          (progn
                    (clim:accept 'string :stream stream :view view :prompt nil :history nil :replace-input t))))
          (string
           (clim:accept 'string :stream stream :view view :prompt nil
                                :history nil :replace-input t
                                :default inner-object :insert-default t)))
      (t
       (log:trace "other command type: obj=~s type=~s ev=~s options=~s" object type event options)
       (funcall (cdar clim:*input-context*) object type event options)))))

(defmethod clim:read-frame-command ((frame main-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (clim:accept 'string-or-command :stream stream :prompt nil)
    (log:info "Got input: object=~s, type=~s" object type)
    (cond
      ((null object)
       nil)
      ((eq type 'string)
       (log:info "Got string: ~s" object))
      ((and (listp type) (eq (car type) 'clim:command))
       (values object type)))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(clim:define-command (cmd-foo :name "Foo Command" :menu t :command-table foo-commands)
    ()
  (log:info "Test message"))
