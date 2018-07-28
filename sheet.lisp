(defpackage :sheet-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :sheet-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
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
                          (etypecase ,output-record
                            (clim:output-record (clim:output-record-position ,output-record))
                            (clim:region (clim:bounding-rectangle-position ,output-record)))
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

(defun move-rec (output-record dx dy)
  (dimension-bind (output-record :x old-x :y old-y)
    (setf (clim:output-record-position output-record)
          (values (+ dx old-x)
                  (+ dy old-y)))))

(defun gesture-modifier-p (gesture modifiers)
  (let ((bitmap (cond
                  ((keywordp modifiers) (clim:make-modifier-state modifiers))
                  ((listp modifiers) (loop
                                       for m in modifiers
                                       for state = (clim:make-modifier-state m)
                                       for b = state then (logior b state)
                                       finally (return b)))
                  (t (error "Unexpected modifiers value: ~s" modifiers)))))
    (eql bitmap (logand (clim::event-modifier-state gesture) bitmap))))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))


(defparameter *button-border* (clim:make-rgb-color 0.7 0.7 0.7))
(defparameter *button-background* (clim:make-rgb-color 0.95 0.95 0.95))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass button ()
    ((label :initarg :label
            :reader button/label)
     (tag   :initarg :tag
            :reader button/tag))))

(clim:define-presentation-method clim:present (obj (type button) stream view &key)
  (let ((stream *standard-output*))
    (clim:with-room-for-graphics (stream)
      (clim:surrounding-output-with-border (stream :ink *button-border*
                                                   :background *button-background*
                                                   :move-cursor nil
                                                   :padding 2)
        (clim:with-text-style (stream (clim:make-text-style :sans-serif :roman 14))
          (format stream "~a" (button/label obj)))))))

(defun call-with-temp-form (stream fn)
  (let ((old-cursor-pos (multiple-value-list (clim:stream-cursor-position stream))))
    (unwind-protect
         (funcall fn)
      (setf (clim:stream-cursor-position stream) (apply #'values old-cursor-pos)))))

(defmacro with-temp-form ((stream) &body body)
  (check-type stream symbol)
  `(call-with-temp-form ,stream (lambda () ,@body)))

(defun call-with-interactive-form (stream builder-fn loop-fn)
  (with-temp-form (stream)
    (let ((active t))
      (let ((rec (clim:updating-output (stream)
                   (when active
                     (funcall builder-fn)
                     (format stream "~%")
                     (present-to-stream (make-instance 'button :label "OK" :tag :ok) stream)
                     (present-to-stream (make-instance 'button :label "Cancel" :tag :cancel) stream)))))
        (unwind-protect
             (funcall loop-fn rec)
          (setq active nil)
          (clim:redisplay rec stream))))))

(defmacro with-interactive-form ((stream record-name) builder &body body)
  (check-type stream symbol)
  (alexandria:with-gensyms (rec)
    `(call-with-interactive-form ,stream
                                 (lambda () ,builder)
                                 (lambda (,rec)
                                   (let ((,record-name ,rec))
                                     ,@body)))))

(defgeneric sheet-rows (sheet))
(defgeneric sheet-cols (sheet))
(defgeneric sheet-value-at-pos (sheet row col))
(defgeneric sheet-render-value (sheet row col stream))
(defgeneric sheet-render-col-title (sheet col stream))
(defgeneric sheet-render-row-title (sheet row stream))

(defclass sheet-edit-value ()
  ((col :initarg :col
        :reader sheet-edit-value/col)
   (row :initarg :row
        :reader sheet-edit-value/row)))

(defun sheet-edit (stream sheet)
  (let ((curr-row 0)
        (curr-col 0)
        (margin-width 5))
    (with-interactive-form (stream rec)
        (clim:formatting-table (stream)
          (clim:formatting-row (stream)
            (clim:formatting-cell (stream)
              (format stream " "))
            (loop
              for i from 0 below (sheet-rows sheet)
              do (clim:formatting-cell (stream :align-x :center :align-y :center)
                   (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold nil))
                     (sheet-render-col-title sheet i stream)))))
          (loop
            for row from 0 below (sheet-rows sheet)
            do (clim:formatting-row (stream)
                 (clim:formatting-cell (stream :align-x :center :align-y :center)
                   (clim:with-text-style (stream (clim:make-text-style :sans-serif :bold nil))
                     (sheet-render-row-title sheet row stream)))
                 (loop
                   for col from 0 below (sheet-cols sheet)
                   do (let ((selected (and (= curr-row row) (= curr-col col))))
                        (clim:formatting-cell (stream :align-x :center :align-y :center)
                          (clim:updating-output (stream :unique-id (list 'matrix-edit row col)
                                                        :id-test #'equal
                                                        :cache-value (list row col selected (sheet-value-at-pos sheet row col))
                                                        :cache-test #'equal)
                            (clim:with-output-as-presentation (stream (make-instance 'sheet-edit-value
                                                                                     :row row
                                                                                     :col col)
                                                                      'sheet-edit-value
                                                                      :allow-sensitive-inferiors nil)
                              (let ((value-rec (clim:with-output-to-output-record (stream)
                                                 (sheet-render-value sheet row col stream))))
                                (dimension-bind (value-rec :x x1 :y y1 :right right :bottom bottom)
                                  (let ((x2 (+ right (* margin-width 2)))
                                        (y2 (+ bottom (* margin-width 2))))
                                    (cond (selected
                                           (clim:draw-rectangle* stream x1 y1 x2 y2 :filled t :ink clim:+lightgrey+))
                                          (t
                                           (clim:draw-rectangle* stream x1 y1 x2 y2 :filled t :ink clim:+white+)))))
                                (move-rec value-rec margin-width margin-width)
                                (clim:stream-add-output-record stream value-rec))))))))))
      ;; Main loop for the form
      (labels ((redraw ()
                 (clim:redisplay rec stream))
               (move-x (delta)
                 (setf curr-col (max (min (+ curr-col delta) (1- (sheet-cols sheet))) 0))
                 (redraw))
               (move-y (delta)
                 (setf curr-row (max (min (+ curr-row delta) (1- (sheet-rows sheet))) 0))
                 (redraw)))
        (loop
          named matrix-edit-main-loop
          for gesture = (clim:with-input-context ('(or sheet-edit-value button) :override nil)
                            (object type)
                            (clim:read-gesture :stream stream)
                          (sheet-edit-value
                           (log:info "value clicked = ~s" object))
                          (button
                           (log:info "button clicked: ~s" object)))
          when (typep gesture 'clim:key-press-event)
            do (let ((event-name (clim:keyboard-event-key-name gesture)))
                 (log:info "event name: ~s" event-name)
                 (if (gesture-modifier-p gesture :control)
                     (case event-name
                       (:|p| (move-y -1))
                       (:|n| (move-y 1))
                       (:|b| (move-x -1))
                       (:|f| (move-x 1)))
                     (case event-name
                       (:up (move-y -1))
                       (:down (move-y 1))
                       (:left (move-x -1))
                       (:right (move-x 1))
                       (:escape
                        (return-from matrix-edit-main-loop nil))))))))))

(clim:define-application-frame main-frame ()
  ()
  (:panes (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       interaction-pane))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'main-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(defclass test-matrix ()
  ((content :reader test-matrix/content)))

(defmethod initialize-instance :after ((obj test-matrix) &key)
  (let ((array (make-array '(5 7))))
    (loop
      for row from 0 below (array-dimension array 0)
      do (loop
           for col from 0 below (array-dimension array 1)
           do (log:info "setting: ~a,~a" row col)
           do (setf (aref array row col) (format nil "pos:~a,~a" row col))))
    (setf (slot-value obj 'content) array)))

(defmethod sheet-rows ((sheet test-matrix))
  (array-dimension (test-matrix/content sheet) 0))

(defmethod sheet-cols ((sheet test-matrix))
  (array-dimension (test-matrix/content sheet) 1))

(defmethod sheet-value-at-pos ((sheet test-matrix) row col)
  (aref (test-matrix/content sheet) row col))

(defmethod sheet-render-value ((sheet test-matrix) row col stream)
  (let ((rec (clim:with-output-to-output-record (stream)
               (format stream "~a" (aref (test-matrix/content sheet) row col)))))
    (clim:stream-add-output-record stream rec)))

(defmethod sheet-render-col-title ((sheet test-matrix) col stream)
  (format stream "~a" (1+ col)))

(defmethod sheet-render-row-title ((sheet test-matrix) row stream)
  (format stream "~a" (1+ row)))

(define-main-frame-command (open-sheet :name "Open sheet")
    ()
  (let ((stream *standard-input*)
        (matrix (make-instance 'test-matrix)))
    (sheet-edit stream matrix)))
