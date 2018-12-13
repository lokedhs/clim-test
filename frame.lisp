(defpackage :clim-test.frame
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.frame)

(clim:define-application-frame foo-frame ()
  ()
  (:panes #+nil (content :application
                   :display-function 'display-content
                   :display-time t
                   :incremental-redisplay t)
          (content (clim:make-pane 'clim:basic-pane))
          (interaction-pane :interactor))
  (:layouts (default content
                     interaction-pane)))

(defun display-content (frame stream)
  (declare (ignore frame))
  (clim:with-room-for-graphics (stream)
    (clim:draw-rectangle* stream 10 10 400 400 :filled nil)))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))

(define-foo-frame-command (set-height-command :name "Height")
    ((height 'integer))
  (let ((stream (clim:find-pane-named clim:*application-frame* 'content)))
    (clim:changing-space-requirements (:resize-frame t)
      (clim:change-space-requirements stream :height height))))

(define-foo-frame-command (set-width-command :name "Width")
    ((width 'integer))
  (let ((stream (clim:find-pane-named clim:*application-frame* 'content)))
    (clim:changing-space-requirements (:resize-frame t)
      (clim:change-space-requirements stream :width width))))

(define-foo-frame-command (set-size-command :name "Size")
    ((width 'integer)
     (height 'integer))
  (let ((stream (clim:find-pane-named clim:*application-frame* 'content)))
    (clim:changing-space-requirements (:resize-frame t)
      (clim:change-space-requirements stream :width width :height height))))
