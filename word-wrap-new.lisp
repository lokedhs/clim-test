(defpackage :clim-test.wordwrap-new
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.wordwrap-new)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word-wrap implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+sbcl
(defun split-word (string)
  (sb-unicode:words string))

#-sbcl
(defun split-word (string)
  (when (plusp (length string))
    (loop
      with start = 0
      with last-char-space-p = nil
      with result = nil
      for i from 0 below (length string)
      for ch = (aref string i)
      for space-p = (cl-unicode:has-property ch "White_Space")
      when (or space-p last-char-space-p)
        do (progn
             (when (< start i)
              (push (subseq string start i) result))
             (setq start i)
             (setq last-char-space-p space-p))
      finally (progn
                (push (subseq string start) result)
                (return (reverse result))))))

(defvar *word-wrap-x* nil)
(defvar *word-wrap-right-margin* nil)
(defvar *word-wrap-height* nil)
(defvar *word-wrap-line* nil)

(defun font-height (stream)
  (multiple-value-bind (width ascent descent left right font-ascent font-descent)
      (clim-clx::font-text-extents (clim-clx::text-style-to-x-font (clim:port (clim:sheet-medium stream))
                                                                   (clim:pane-text-style stream))
                                   "M")
    (declare (ignore width ascent descent left right))
    (log:info "a=~s d=~s" font-ascent font-descent)
    (+ font-ascent font-descent)))

(defmacro with-word-wrap ((stream &key right-margin height) &body body)
  (alexandria:once-only (stream right-margin height)
    `(let ((*word-wrap-x* 0)
           (*word-wrap-right-margin* (or ,right-margin (clim:rectangle-width (clim:pane-viewport-region ,stream))))
           (*word-wrap-height* (or ,height (font-height stream))))
       (let ((*word-wrap-line* *word-wrap-height*))
         ,@body))))

(defun split-string-at-right-margin (stream parts right-margin)
  (loop
    with curr-string = ""
    with part = parts
    with width = 0
    while (not (endp part))
    while (let* ((next-string (format nil "~a~a" curr-string (car part)))
                 (w (clim:text-size stream next-string)))
            (cond ((<= w right-margin)
                   (setq curr-string next-string)
                   (setq part (cdr part))
                   (setq width w)
                   t)
                  (t
                   nil)))
    finally (return (values curr-string part width))))

(defun word-wrap-draw-one-line (stream parts)
  (let ((start *word-wrap-x*)
        (right-margin *word-wrap-right-margin*)
        (line *word-wrap-line*))
    (multiple-value-bind (initial more-parts width)
        (split-string-at-right-margin stream parts (- right-margin start))
      (log:info "drawing ~s at ~s" initial start)
      (clim:draw-text* stream initial start line)
      (cond (more-parts
             (setq *word-wrap-x* 0)
             (incf *word-wrap-line* *word-wrap-height*))
            (t
             (incf *word-wrap-x* width)))
      more-parts)))

(defun word-wrap-draw-string (stream string)
  (let ((parts (split-word string)))
    (loop
      while parts
      do (setq parts (word-wrap-draw-one-line stream parts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test harness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun present-horizontal-separator (stream)
  (let ((width (clim:rectangle-width (clim:pane-viewport-region stream))))
    (multiple-value-bind (x y)
        (clim:cursor-position (clim:stream-text-cursor stream))
      (declare (ignore x))
      (let ((new-y (+ y 10)))
        (clim:draw-line stream
                        (clim:make-point 20 new-y)
                        (clim:make-point (- width 20) new-y)))
      #+nil(setf (clim:cursor-position (clim:stream-text-cursor stream))
            (values x (+ 20 y)))
      (clim:stream-increment-cursor-position stream 0 20))))

(clim:define-application-frame foo-frame ()
  ((content :type list
            :initarg :content
            :initform nil
            :accessor foo-frame/content))
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

(defun display-text-content (frame stream)
  (let ((content (foo-frame/content frame)))
    #+nil
    (let ((coll (make-instance 'word-wrap-collector :stream stream)))
      (dolist (s content)
        (append-word-wrap-content coll s)))
    (when content
      (with-word-wrap (stream)
        (dolist (s content)
          (word-wrap-draw-string stream s))))))

(define-foo-frame-command (add-string :name "Add")
    ((text 'string))
  (let ((frame clim:*application-frame*))
    (setf (foo-frame/content frame)
          (append (foo-frame/content frame)
                  (list text)))))

(define-foo-frame-command (add-multi :name "Multi")
    ((n 'integer))
  (let ((frame clim:*application-frame*))
    (setf (foo-frame/content frame)
          (append (foo-frame/content frame)
                  (list (with-output-to-string (s)
                          (loop
                            for i from 0 below n
                            for first = t then nil
                            do (format s "~:[ ~;~]~r" first i))))))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))
