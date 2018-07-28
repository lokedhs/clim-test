(defpackage :font-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :font-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (progn
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3))
  (unless (find-package "CLIM")
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))
 
(setq clim-freetype:*enable-autohint* t)

(defclass direct-font-text-style (clim:standard-text-style)
  ())

(defun find-direct-file-freetype-font (port text-style)
  (multiple-value-bind (family face size)
      (clim:text-style-components text-style)
    (let* ((family-obj (clim-freetype::find-font-family port family))
           (face-obj (alexandria:ensure-gethash face (clim-freetype::freetype-font-family/faces family-obj)
                                                (make-instance 'clim-freetype::freetype-font-face
                                                               :family family-obj
                                                               :name face
                                                               :file family))))
      (make-instance 'clim-freetype::freetype-font
                     :port port
                     :face face-obj
                     :size (etypecase size
                             (keyword (or (getf clim-clx::*clx-text-sizes* size) 12))
                             (number size)
                             (null 12))))))

(defmethod clim-clx::lookup-text-style-to-x-font ((port clim-clx::clx-port)
                                                  (renderer clim-freetype::freetype-font-renderer)
                                                  (text-style direct-font-text-style))
  (let ((x (or (clim:text-style-mapping port text-style)
               (setf (clim:text-style-mapping port text-style)
                     (find-direct-file-freetype-font port text-style)))))
    (log:info "found font: ~s" x)
    x))

(defmethod clim-clx::lookup-text-style-to-x-font :around ((port clim-clx::clx-port)
                                                          (renderer clim-freetype::freetype-font-renderer)
                                                          (text-style clim:standard-text-style))
  (if (alexandria:starts-with-subseq "/" (string (clim:text-style-family text-style)))
      (let ((x (or (clim:text-style-mapping port text-style)
                   (setf (clim:text-style-mapping port text-style)
                         (find-direct-file-freetype-font port text-style)))))
        x)
      (call-next-method)))

(defclass text-content-view (clim:view)
  ())

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (mcclim-fontconfig:app-font-add-dir #p"/home/elias/prog/maxima-client/fonts/gyre-termes/")
  (mcclim-fontconfig:app-font-add-dir #p"/home/elias/prog/maxima-client/fonts/latin-modern/")
  (mcclim-fontconfig:app-font-add-dir #p"/home/elias/prog/maxima-client/fonts/asana-math/")
  (mcclim-fontconfig:app-font-add-dir #p"/home/elias/prog/maxima-client/fonts/neo-euler/")
  (mcclim-fontconfig:app-font-add-dir #p"/home/elias/prog/maxima-client/fonts/tex/")
  (let ((size 20)
        (fonts (loop
                 for f in (mcclim-fontconfig:font-list nil '(:family :style :file))
                 for family = (cdr (assoc :family f))
                 for style = (cdr (assoc :style f))
                 for file = (cdr (assoc :file f))
                 when (alexandria:starts-with-subseq "/home/elias/prog/maxima-client/fonts/tex" file)
                   collect (list family style file))))
    (loop
      for (family style file) in fonts
      do (format stream "~a/~a/~a " family style (pathname-name file))
      do (clim:with-text-style (stream (make-instance 'direct-font-text-style
                                                      :text-family file :text-face family :text-size size))
           (format stream "ABCIZ abciz 012345 ~c ~c ~c ~c ~c ~c [x] (p) % ~c ~%"
                   #\GREEK_CAPITAL_LETTER_SIGMA
                   #\INTEGRAL
                   #\GREEK_CAPITAL_LETTER_PI
                   #\GREEK_SMALL_LETTER_PI
                   #\INFINITY
                   #\DOT_OPERATOR
                   #\SQUARE_ROOT)))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 1400
                                            :height 1000)))
    (clim:run-frame-top-level frame)))
