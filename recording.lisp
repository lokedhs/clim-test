(defpackage :recording-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :recording-test)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(clim:define-presentation-type foo
    ()
  :inherit-from t)

(clim:define-presentation-type foo-data
    ()
  :inherit-from t)

(defclass foo ()
  ())

(defun add-recording (stream)
  ;; Create the underlying object for the presentation
  (let ((x (make-instance 'foo))
        (p (make-instance 'clim:standard-sequence-output-record)))
    (clim:with-output-as-presentation (stream x 'foo-data
                                              :view (clim:stream-default-view stream)
                                              :allow-sensitive-inferiors t
                                              :record-type 'clim:standard-presentation
                                              :parent p)
      (clim:with-room-for-graphics (stream :first-quadrant nil)
        (clim:draw-text* stream "Foo" 30 30)
        (clim:draw-line* stream 40 40 100 60 :ink clim:+blue+)))
    #+nil
    (clim:stream-add-output-record stream p)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "Test content~%")
  (add-recording stream))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(clim:define-command (add-presentation :name "Add presentation" :menu t :command-table foo-frame)
    ()
  (add-recording (clim:find-pane-named clim:*application-frame* 'interaction-pane)))
