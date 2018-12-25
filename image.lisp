(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "log4cl")
    (ql:quickload "mcclim")))

(defpackage :clim-test.imgtest
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.imgtest)

(clim:define-application-frame foo-frame ()
  ()
  (:panes (content :application
                   :display-function 'display-content
                   :display-time t
                   :incremental-redisplay t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       (9/10 content)
                       (1/10 interaction-pane)))))

(defun display-content (frame stream)
  (declare (ignore frame))
  (let ((image (clim:make-pattern-from-bitmap-file "plotting1.png")))
    (labels ((make-rec (title)
               (clim:with-output-to-output-record (stream)
                 (clim:with-room-for-graphics (stream :first-quadrant nil)
                   (clim:draw-rectangle* stream 0 0 (clim:pattern-width image) (clim:pattern-height image) :ink image)
                   (clim:draw-text* stream title (+ 50 (clim:pattern-width image)) 100)))))
      (let ((rec (make-rec "Position 10")))
        (setf (clim:output-record-position rec) (values 10 10))
        (clim:stream-add-output-record stream rec))
      (let ((rec (make-rec "Position 400.5")))
        ;; Note the additional half-pixel in the y-coordinate. If the
        ;; y-coordinate is an integer, then everything works
        ;; correctly.
        (setf (clim:output-record-position rec) (values 10 400.5))
        (clim:stream-add-output-record stream rec))
      (let ((rec (make-rec "Position 800.49999")))
        (setf (clim:output-record-position rec) (values 10 800.49999))
        (clim:stream-add-output-record stream rec)))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame :width 900 :height 800)))
    (clim:run-frame-top-level frame)))
