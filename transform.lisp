(defpackage :transform-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :transform-test)

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
    ((include-identity-transformation-p :initarg :include-identity-transformation-p
                                        :reader foo/include-identity-transformation-p)))

  (defclass bar ()
    ()))

(clim:define-application-frame main-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defun xdisplay-text-content (frame stream)
  (declare (ignore frame))
  (labels ((draw ()
             (clim:with-rotation (stream (- (/ pi 8)) (clim:make-point 30 50))
               (clim:draw-text* stream "Foo1" 30 50)
               (clim:draw-line* stream 30 50 90 50))))
    #+nil
    (draw)
    (let ((rec (clim:with-output-to-output-record (stream)
                 (draw))))
      (multiple-value-bind (x y)
          (clim:output-record-position rec)
        (setf (clim:output-record-position rec)
              (values (+ x 50)
                      y)))
      (clim:stream-add-output-record stream rec))))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "The below presentation shows the~%text displayed in the correct place:~%~%")
  (present-to-stream (make-instance 'foo :include-identity-transformation-p t) stream)

  (format stream "~%Now, the same
presentation is displayed, this time
without the identity transformation.
Note how the line is drawn
place but the text disappears. In fact,
it hasn't disappeared. It's just been moved
outside the visible area since the transformation
has been applied twice:~%~%")
  (present-to-stream (make-instance 'foo :include-identity-transformation-p nil) stream)

  (format stream "~%======================
A different way to illustrate the same issue is to
try to draw rotated text in a presentation method.
The below presentation method attempts to draw
a string that is rotated 90 degrees to the left.

Note how the string \"Foo\" end up in the top-right
of the screen.~%")
  (present-to-stream (make-instance 'bar) stream))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(clim:define-presentation-method clim:present (obj (type foo) stream (view t) &key)
  (clim:with-drawing-options (stream :ink clim:+red+)
    (format stream "Before text~%")
    (labels ((draw ()
               (clim:draw-line* stream 20 50 100 50)
               (clim:draw-text* stream "Foo" 20 50)))
      (clim:with-room-for-graphics (stream)
        (if (foo/include-identity-transformation-p obj)
            (clim:with-identity-transformation (stream)
              (draw))
            (draw))))
    (format stream "After text~%")))

(clim:define-presentation-method clim:present (obj (type bar) stream (view t) &key)
  (clim:with-drawing-options (stream :ink clim:+purple+)
    (format stream "Before the graphics test~%")
    (clim:with-room-for-graphics (stream)
      (clim:with-identity-transformation (stream)
        (clim:draw-rectangle* stream 0 0 100 100 :filled nil)
        (log:info "Before DRAW-TEXT")
        (clim:draw-text* stream "Foo1" 30 50 :transformation (clim:make-rotation-transformation* (- (/ pi 2)) 30 50))
        (log:info "After DRAW-TEXT")))
    (format stream "After the graphics test~%")))
