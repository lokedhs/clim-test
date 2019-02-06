(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "log4cl")
    (ql:quickload "mcclim")))

(defpackage :clim-test.presentations
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.presentations)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass foo ()
    ())
  (defclass foo-view ()
    ()))

(defvar +foo-view+ (make-instance 'foo-view))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (content :application
                   :display-function 'display-content
                   :display-time t
                   :incremental-redisplay t
                   :default-view +foo-view+)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       (9/10 content)
                       (1/10 interaction-pane)))))

(clim:define-presentation-method clim:present (obj (type foo) stream (view foo-view) &key)
  (log:info "Presenting obj: ~s" obj)
  (let ((x1 10)
        (y1 20)
        (x2 100)
        (y2 150)
        (n 10))
    (clim:draw-line* stream x1 y1 (+ x1 n) y1)
    (clim:draw-line* stream x1 y1 x1 (+ y1 n))
    (clim:draw-line* stream x2 y2 x2 (- y2 n))
    (clim:draw-line* stream x2 y2 (- x2 n) y2)
    (clim:draw-line* stream x1 y1 x2 y2)))

(defclass special-highlight (clim:standard-presentation)
  ())

(defun display-content (frame stream)
  (declare (ignore frame))
  (format stream "before~%")
  (let ((obj (make-instance 'foo)))
    (clim:with-room-for-graphics (stream :first-quadrant nil :move-cursor t)
      (let ((rec (clim:with-output-to-output-record (stream)
                   (clim:stream-present stream obj (clim:presentation-type-of obj)
                                        :single-box t :record-type 'special-highlight))))
        (multiple-value-bind (rx1 ry1 rx2 ry2)
            (clim:bounding-rectangle* rec)
          (log:info "Output record rect: (~s,~s)-(~s,~s)" rx1 ry1 rx2 ry2)
          (clim:stream-add-output-record stream rec)
          (clim:draw-rectangle* stream rx1 ry1 rx2 ry2 :filled nil :ink clim:+blue+)))))
  (format stream "after~%")
  (clim:draw-rectangle* stream 10 200 12 202 :filled nil :ink clim:+red+))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame :width 900 :height 800)))
    (clim:run-frame-top-level frame)))

(clim:define-presentation-to-command-translator select-foo
    (foo cmd-show-foo foo-frame)
    (obj)
  (list obj))

(define-foo-frame-command (cmd-show-foo :name "Show")
    ((obj 'foo :prompt "Instance"))
  (format t "Object: ~s" obj))

(defvar cl-user::*force-break*)

(defmethod clim:highlight-output-record ((record special-highlight) stream state)
  (flet ((r (n)
           n))
    (clim:with-identity-transformation (stream)
      (multiple-value-bind (x1 y1 x2 y2)
          (clim:bounding-rectangle* record)
        (log:info "Bounding rectangle: (~s,~s)-(~s,~s)" x1 y1 x2 y2)
        (let ((medium (clim:sheet-medium stream)))
          (ecase state
            (:highlight
             (clim:draw-rectangle* medium (r x1) (r y1) (r x2) (r y2) :filled t :ink clim:+green+)
             (clim:replay-output-record (clim:stream-output-history stream) stream (clim:make-rectangle* x1 y1 x2 y2)))
            (:unhighlight
             (let ((cl-user::*force-break* nil))
               (clim:repaint-sheet stream (clim:bounding-rectangle record))))))))))
