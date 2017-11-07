(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim")
    (ql:quickload "log4cl")))

(defpackage :clim-test
  (:use :cl))

(in-package :clim-test)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun display-content (frame stream)
  (format stream "This is some text~%")
  (let ((climi::*pane-realizer* (clim:frame-manager frame)))
    (clim:surrounding-output-with-border (stream :ink (clim:make-rgb-color 0 0 0))
      (clim:with-output-as-gadget (stream)
        (clim:make-pane 'clim:text-field-pane :id 'name :value "Some text" :editable-p t)))
    (format stream "~%~%Option pane:~%")
    (clim:with-output-as-gadget (stream)
      (clim:make-pane 'clim:option-pane :value "Foo" :items '("X" "y" "foo" "Blah" "Foo" "hello" "blappe")))
    (terpri stream)
    (clim:surrounding-output-with-border (stream :ink (clim:make-rgb-color 0 0 0))
      (clim:with-output-as-gadget (stream)
        (clim:make-pane 'clim:text-editor-pane :id 'name2 :value "Some other text oifwj eiojwe rgiojo vfrokiervj weiovj weiojv oiwejvwioeweiovjweo" :editable-p t)))))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (content :application
                   :display-function 'display-content)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
