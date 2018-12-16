(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (ql:quickload "log4cl")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim")))

(defpackage :clim-test.font-size
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.font-size)

(clim:define-application-frame foo-frame ()
  ()
  (:panes (documentation-content :application
                                 :display-function 'display-content
                                 :display-time t
                                 :incremental-redisplay t))
  (:layouts (default (clim:vertically ()
                       documentation-content))))

(defun display-font-size (stream size y)
  (clim:with-text-style (stream (clim:make-text-style "DejaVu Sans" "Book" size))
    (let ((font-ascent (climb:text-style-ascent (clim:pane-text-style stream) stream)))
      (clim:draw-text* stream (format nil "Text size: ~s, font ascent: ~s" size font-ascent) 20 y))))

(defun display-content (frame stream)
  (declare (ignore frame))
  (loop
    for size in '(8 12 14 16 18 20 24 26)
    for y = 20 then (+ y (* size 2))
    do (display-font-size stream size y)))

(defun display-popup-content (frame stream)
  (declare (ignore frame))
  (format stream "This is the content of the popup~%")
  (clim:stream-present stream "popup-frame" 'foo-type))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
