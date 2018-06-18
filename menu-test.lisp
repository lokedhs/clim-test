(defpackage :menu-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :menu-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "log4cl")
    (ql:quickload "mcclim")))

(clim:define-application-frame foo-frame ()
  ((content :type string
            :initarg :content
            :initform ""
            :accessor foo-frame/content))
  (:panes (text-content :application
                        :display-function 'display-text-content
                        :redisplay-on-resize-p t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (format stream "Some text"))

(define-foo-frame-command (menu-command :name "Menu")
    ()
  (multiple-value-bind (object event)
      (clim:with-menu (menu nil :label "Foo" :scroll-bars :vertical)
        (log:info "menu=~s" menu)
        (climi::enable-menu menu)
        (format menu "This is some text in the menu")
        (clim:accept 'string :stream menu))
    (log:info "Result from with-menu: object=~s event=~s" object event)))

(defgeneric draw-popup-value (stream value selected))

(defmethod draw-popup-value (stream (value string) selected)
  (clim:draw-text* stream value 0 0 :ink (if selected clim:+green+ clim:+black+)))

(defun display-popup-content (stream values sel)
  (clim:updating-output (stream)
    (loop
      for value in values
      for i from 0
      for element-selected = (eql i sel)
      do (clim:updating-output (stream :unique-id i
                                       :cache-value (list i element-selected)
                                       :cache-test #'equal)
           (clim:with-room-for-graphics (stream :first-quadrant nil)
             (clim:draw-text* stream value 0 0 :ink (if element-selected clim:+green+ clim:+black+)))))))

(defun display-popup-content2 (frame stream)
  (declare (ignore frame))
  (format stream "Some content here~%"))

(define-foo-frame-command (pane-command :name "Pane")
    ()
  ;; (clim:pane-frame associated-window)
  (let ((values '("Foo" "Bar" "Test entry" "Another entry" "Yet another one")))
    (let* ((associated-frame clim:*application-frame*)
           (fm (clim:frame-manager associated-frame)))
      (clim:with-look-and-feel-realization (fm associated-frame)
        (let* ((menu-pane (clim:make-pane-1 fm associated-frame 'clim:application-pane))
               (menu-container (clim:scrolling (:scroll-bar :vertical) menu-pane))
               (frame (clim-internals::make-menu-frame (clim-internals::raising ()
                                                         (clim:labelling (:label "Completions" :name 'label :label-alignment :top)
                                                           menu-container))
                                                       :left nil
                                                       :top nil)))
          (clim:adopt-frame fm frame)
          (unwind-protect
               (progn
                 (setf (clim:stream-end-of-line-action menu-pane) :allow)
                 (setf (clim:stream-end-of-page-action menu-pane) :allow)
                 ;; Draw menu
                 (clim:enable-frame frame)
                 (display-popup-content menu-pane values 0)
                 (loop
                   named control-loop
                   ;;with selected-index = 0
                   for gesture = (clim:with-input-context ('string :override nil)
                                     (object type)
                                     (clim:read-gesture :stream menu-pane)
                                   (string (log:info "got string: ~s" object)))
                   do (log:info "Got gesture: ~s" gesture)
                   when (or (eql gesture #\Newline)
                            (eql gesture #\Tab))
                     do (return-from control-loop 'result-here)
                   when (typep gesture 'clim:key-press-event)
                     do (case (clim:keyboard-event-key-name gesture)
                          (:up (log:info "up"))
                          (:down (log:info "down"))
                          (:escape (return-from control-loop nil)))))
            (clim:disown-frame fm frame)))))))
