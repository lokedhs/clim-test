(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim")
    (ql:quickload "log4cl")))

(defpackage :clim-test
  (:use :cl))

(in-package :clim-test)

(defparameter *out* *debug-io*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *editor-pane-border* (clim:make-rgb-color 0 0 0))

  (defclass post-content ()
    ())

  (defclass post-text-record (climi::accepting-values-record)
    ((editor :accessor post-text-record/editor))))

(defmethod climi::select-query (stream query (record post-text-record))
  )

(defmethod climi::deselect-query (stream query (record post-text-record))
  )

(defmethod climi::finalize-query-record (query (record post-text-record))
  (let ((editor (post-text-record/editor record)))
    (when editor
      (setf (climi::value query) (clim:gadget-value editor)))))

(defvar *editor* nil)

(defun make-word-wrapped-text-editor (&key ncolumns nlines value)
  (let* ((editor (clim:make-pane 'clim:text-editor
                                 :ncolumns ncolumns
                                 :nlines nlines
                                 :value value))
         (view (clim:view (climi::substrate editor))))
    (setf (drei:auto-fill-mode view) t)
    (setf (drei:auto-fill-column view) ncolumns)
    editor))

(clim:define-presentation-method clim:accept-present-default ((type post-content)
                                                              stream (view t)
							      default default-supplied-p present-p query-identifier)
  (let* ((editor nil)
	 (record (clim:updating-output (stream :unique-id query-identifier
                                               :cache-value nil
		                               :record-type 'post-text-record)
                   (clim:surrounding-output-with-border (stream :ink *editor-pane-border* :padding 2)
	            (clim:with-output-as-gadget (stream)
	              (setq editor (make-word-wrapped-text-editor :ncolumns 40
                                                                  :nlines 8
                                                                  :value (if default-supplied-p default ""))))))))
    (when editor
      (setq *editor* editor)
      (setf (post-text-record/editor record) editor))
    record))

#+nil
(clim:define-presentation-method clim:accept ((type post-content) stream (view t) &key)
  (clim:with-output-as-gadget (stream)
    (clim:make-pane 'clim:text-editor-pane :ncolumns 40 :nlines 8)))

(defun accepting-post (&key (stream *query-io*) (own-window nil))
  (let (content)
    (clim:accepting-values (stream :resynchronize-every-pass t :own-window own-window)
      (setq content (clim:accept 'post-content :prompt "Post Content" :stream stream :default "some text"))
      (fresh-line stream))
    content))

(defun display-description (frame stream)
  (declare (ignore frame))
  (format stream "Test program for accept"))

(clim:define-application-frame mastodon-frame ()
  ()
  (:panes (description-pane :application
                            :display-function 'display-description)
          (interaction-pane :interactor))
  (:layouts (default description-pane
                     interaction-pane)))

(define-mastodon-frame-command (post :name "Post")
    ()
  (let ((content (accepting-post)))
    (format *out* "Got content: ~s~%" content)))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'mastodon-frame
                                            :width 600 :height 400)))
    (clim:run-frame-top-level frame)))
