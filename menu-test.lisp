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

(defmacro dimension-bind ((output-record &key
                                           ((:width width-sym)) ((:height height-sym))
                                           ((:x x-sym)) ((:y y-sym))
                                           ((:right right-sym)) ((:bottom bottom-sym))
                                           ((:baseline baseline-sym)))
                          &body body)
  (alexandria:with-gensyms (width height x y)
    (alexandria:once-only (output-record)
      (labels ((make-body ()
                 `(progn ,@body))
               (make-baseline ()
                 (if baseline-sym
                     `(let ((,baseline-sym (clim-extensions:output-record-baseline ,output-record)))
                        ,(make-body))
                     (make-body)))
               (make-position-form ()
                 (if (or x-sym y-sym right-sym bottom-sym)
                     `(multiple-value-bind (,x ,y)
                          (clim:output-record-position ,output-record)
                        (declare (ignorable ,x ,y))
                        (let (,@(if x-sym `((,x-sym ,x)))
                              ,@(if y-sym `((,y-sym ,y)))
                              ,@(if right-sym `((,right-sym (+ ,x ,width))))
                              ,@(if bottom-sym `((,bottom-sym (+ ,y ,height)))))
                          ,(make-baseline)))
                     (make-baseline))))
        (if (or width-sym height-sym right-sym bottom-sym)
            `(multiple-value-bind (,width ,height)
                 (clim:rectangle-size ,output-record)
               (declare (ignorable ,width ,height))
               (let (,@(if width-sym `((,width-sym ,width)))
                     ,@(if height-sym `((,height-sym ,height))))
                 ,(make-position-form)))
            (make-position-form))))))

(clim:define-application-frame foo-frame ()
  ((content :type list
            :initarg :content
            :initform (list "Foo" "Test" "Some text" "Content" "Some more content")
            :accessor foo-frame/content))
  (:panes (text-content :application
                        :display-function 'display-text-content
                        :incremental-redisplay t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))

(defun display-text-content (frame stream)
  (clim:updating-output (stream)
    (loop
      for s in (foo-frame/content frame)
      for i from 0
      do (clim:updating-output (stream :unique-id i
                                       :cache-value s
                                       :cache-test #'equal)
           (clim:with-room-for-graphics (stream :first-quadrant nil)
             (clim:draw-text* stream s 0 0))))))

(define-foo-frame-command (quit :name "Quit")
    ()
  (clim:frame-exit clim:*application-frame*))

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

(defun display-popup-menu-entry (stream value element-selected)
  (clim:draw-text* stream value 0 0 :ink (if element-selected clim:+green+ clim:+black+)))

(defun display-popup-content2 (frame stream)
  (declare (ignore frame))
  (format stream "Some content here~%"))

(define-foo-frame-command (modify-command :name "Modify")
    ((index integer :prompt "Index")
     (value string :prompt "Value"))
  (let ((frame clim:*application-frame*))
    (setf (nth index (foo-frame/content frame)) value)
    (clim:redisplay-frame-pane frame (clim:find-pane-named frame 'text-content))))

(defclass popup-menu-element ()
  ((value  :initarg :value
           :reader popup-menu-element/value)
   (record :initarg :record
           :accessor popup-menu-element/record)))

(defun make-menu-entry-vector (stream values)
  (let ((vector (make-array (length values))))
    (loop
      with y = 0
      for v in values
      for i from 0
      do (let* ((record (make-instance 'clim:standard-sequence-output-record))
                (inner-record (clim:with-output-to-output-record (stream 'clim:standard-sequence-output-record record)
                                (display-popup-menu-entry stream v (zerop i)))))
           (clim:add-output-record inner-record record)
           (setf (clim:output-record-position record) (values 0 y))
           (clim:stream-add-output-record stream record)
           (let ((element (make-instance 'popup-menu-element :value v :record record)))
             (setf (aref vector i) element))
           (dimension-bind (record :height height)
             (incf y height))))
    vector))

(defun redraw-popup-menu-entry (stream entry element-selected)
  (let* ((record (popup-menu-element/record entry)))
    (dimension-bind (record :y prev-y)
      (clim:clear-output-record record)
      (let ((inner-record (clim:with-output-to-output-record (stream 'clim:standard-sequence-output-record)
                            (display-popup-menu-entry stream (popup-menu-element/value entry) element-selected))))
        (setf (clim:output-record-position inner-record) (values 0 0))
        (clim:add-output-record inner-record record)
        ;;(clim:recompute-extent-for-new-child record inner-record)
        (setf (clim:output-record-position record) (values 0 prev-y))
        (dimension-bind (record :x x :y y :right right :bottom bottom)
          (log:info "AFTER bounds: ~{~f~^, ~}" (list x y right bottom))
          (clim:repaint-sheet stream (clim:make-bounding-rectangle x y right bottom)))))))

(define-foo-frame-command (pane-command :name "Pane")
    ()
  (let ((values (loop for i from 0 below 10 collect (format nil "Value: ~a" i))))
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
                 (let ((entries (make-menu-entry-vector menu-pane values))
                       (selected-index 0))
                   (labels ((move (delta)
                              (let ((new-pos (max (min (+ selected-index delta) (1- (length entries))) 0)))
                                (when (/= new-pos selected-index)
                                  (redraw-popup-menu-entry menu-pane (aref entries selected-index) nil)
                                  (redraw-popup-menu-entry menu-pane (aref entries new-pos) t)
                                  (setq selected-index new-pos)))))
                     (loop
                       named control-loop
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
                              (:up (move -1))
                              (:down (move 1))
                              (:escape (return-from control-loop nil)))))))
            (clim:disown-frame fm frame)))))))
