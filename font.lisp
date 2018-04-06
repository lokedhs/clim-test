(defpackage :font-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :font-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))    (ql:quickload "mcclim")))
 
(defclass text-content-view (clim:view)
  ())

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content
                        :redisplay-on-resize-p t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun display-text-content2 (frame stream)
  (declare (ignore frame))
  (let* ((families (clim-extensions:port-all-font-families (clim:find-port :server-path :clx)))
         (family (find "DejaVu Sans" families :key #'clim-internals::font-family-name :test #'equal)))
    (unless family
      (error "Font family not found"))
    (let* ((faces (clim-extensions:font-family-all-faces family))
           (face (find "Book" faces :key #'clim-internals::font-face-name :test #'equal)))
      (unless face
        (error "Font not found"))
      (let ((text-style #+nil(clim-extensions:font-face-text-style face 14)
                        (clim-internals::make-text-style "DejaVu Sans" "Book" 14)))
        (clim:draw-text* stream (format nil "Test content: ~c" #\GREEK_CAPITAL_LETTER_SIGMA) 100 100
                         :text-style text-style)))))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (let ((text-style #+nil(clim-extensions:font-face-text-style face 14)
                    (clim-internals::make-text-style "Noto Serif" "Regular" 19)))
    (clim:draw-text* stream (format nil "Test content: ~c" #\GREEK_CAPITAL_LETTER_SIGMA) 100 100
                     :text-style text-style)))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
