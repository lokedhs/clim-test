(defpackage :simple-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :simple-test)

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
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content))
  (:layouts (default (clim:vertically ()
                       text-content))))


(defun make-silly-text ()
  (with-output-to-string (s)
    (dotimes (x 40)
      (format s "~c" (code-char (+ (char-code #\a) (random 20)))))))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  
  (dotimes (y 10)
    (clim:draw-line* stream
                     10   (* y 20)
                     1000  (* y 20)
                     :ink clim:+red+
                     :line-thickness 4)
    (clim:draw-text* stream
                     (make-silly-text)
                     10   (* y 20)
                     :text-size 20
                     :ink (nth (random 3) (list clim:+red+ clim:+blue+ clim:+green+)))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))
