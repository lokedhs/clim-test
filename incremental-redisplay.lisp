(defpackage :incremental-redisplay-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :incremental-redisplay-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim")))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(clim:define-application-frame main-frame ()
  ((values :initform (make-array '(2 2) :initial-contents '(("foo" "bar") ("test" "another-message")))
           :reader main-frame/values)
   (output-record :initform nil
                  :accessor main-frame/output-record))
  (:panes (text-content :application
                        :display-function 'display-text-content
                        :incremental-redisplay t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))


(defun display-text-content (frame stream)
  (let ((values (main-frame/values frame)))
    (let ((rec (clim:updating-output (stream)
                 (clim:formatting-table (stream)
                   (loop
                     for row from 0 below (array-dimension values 0)
                     do (clim:formatting-row (stream)
                          (loop
                            for col from 0 below (array-dimension values 1)
                            for value = (aref values row col)
                            do (clim:formatting-cell (stream)
                                 (clim:updating-output (stream :unique-id (list row col)
                                                               :id-test #'equal
                                                               :cache-value value
                                                               :cache-test #'equal)
                                   (clim:draw-text* stream value 0 0))))))))))
      (setf (main-frame/output-record frame) rec))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(define-main-frame-command (update-value :name "Update value")
    ((row 'integer :prompt "Row")
     (col 'integer :prompt "Col")
     (text 'string :prompt "New value"))
  (let* ((frame clim:*application-frame*)
         (values (main-frame/values frame))
         (stream *standard-output*))
    (setf (aref values row col) text)
    (clim:redisplay (main-frame/output-record frame) stream)))
