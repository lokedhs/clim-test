(defpackage :command-input
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :command-input)

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

(defclass bar ()
  ((value :initarg :value
          :initform "abc"
          :accessor bar/value)))

(defmethod print-object ((obj bar) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "value=~s" (bar/value obj))))

(clim:define-presentation-translator bar-to-plain-text (bar plain-text foo-commands)
    (object)
  (bar/value object))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (clim:define-presentation-type plain-text ()
    :inherit-from 'string))

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

(clim:define-presentation-method clim:accept ((type plain-text)
                                              stream (view clim:textual-view)
                                              &key
                                              (default nil defaultp)
                                              (default-type type))
  (let ((result (read-plain-text stream)))
    (cond ((and (equal result "") defaultp)
           (values default default-type))
          (t (values result type)))))

(clim:define-presentation-method clim:accept ((type bar) stream view &key prompt default)
  (let ((result (clim:accept 'plain-text :stream stream :prompt prompt :default default)))
    (values (make-instance 'bar :value result) 'bar)))

(defmethod clim:presentation-replace-input ((stream drei:drei-input-editing-mixin) (obj bar) type view
                                            &key (buffer-start nil buffer-start-p) (rescan nil rescan-p)
                                              query-identifier
                                              for-context-type)
  (declare (ignore query-identifier for-context-type))
  (apply #'clim:presentation-replace-input stream (bar/value obj) 'plain-text view
         (append (if buffer-start-p (list :buffer-start buffer-start) nil)
                 (if rescan-p (list :rescan rescan) nil))))

(clim:define-application-frame main-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:command-table (main-frame :inherit-from (foo-commands)))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "Test message"))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(clim:define-command (cmd-test-accepting-values-no-default :name "Test No Default" :menu t :command-table foo-commands)
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'interaction-pane))
        (result nil))
    (clim:accepting-values (pane)
      (setq result (clim:accept 'bar :stream pane
                                     :prompt "Bar"
                                     :default nil
                                     :insert-default t)))
    (format pane "Got value: ~s" result)))

(clim:define-command (cmd-test-accepting-values-with-default :name "Test With Default" :menu t :command-table foo-commands)
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'interaction-pane))
        (result nil))
    (clim:accepting-values (pane)
      (setq result (clim:accept 'bar :stream pane
                                     :prompt "Bar"
                                     :default (make-instance 'bar :value "testmessage")
                                     :insert-default t)))
    (format pane "Got value: ~s" result)))
