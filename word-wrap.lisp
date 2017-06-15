(defpackage :clim-test.wordwrap
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test.wordwrap)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word-wrap implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun present-text-with-wordwrap (stream text)
  (let ((words (sb-unicode:words text)))
    (loop
      with pane-width = (clim:rectangle-width (clim:pane-viewport-region stream))
      for word in words
      unless (equal word #.(princ-to-string #\Newline))
        do (let ((x (clim:cursor-position (clim:stream-text-cursor stream))))
             (when (> (+ x (clim:stream-string-width stream word)) pane-width)
               (format stream "~%"))
             (format stream "~a" word)))))

(defun present-multiline-with-wordwrap (stream text)
  (loop
    with start = nil
    for i from 0 below (length text)
    if (eql (aref text i) #\Newline)
      do (progn
           (when (and start (> i start))
             (present-text-with-wordwrap stream (subseq text start i))
             (setq start nil))
           (format stream "~%"))
    else
      do (progn
           (when (null start)
             (setq start i)))
    finally (when start
              (present-text-with-wordwrap stream (subseq text start)))))

(defclass word-wrap-stream (clim:standard-encapsulating-stream clim:basic-pane)
  ((buf :type (array character (*))
        :initform (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0)
        :accessor word-wrap-stream/buf)))

(defun %finalise-word-wrap-string (stream)
  (let ((buf (word-wrap-stream/buf stream)))
    (format *debug-io* "Wordwrapping: ~s~%" buf)
    (present-multiline-with-wordwrap (clim:encapsulating-stream-stream stream) buf)
    (setf (fill-pointer buf) 0)))

(defun %word-wrap-push-char (stream char)
  (vector-push-extend char (word-wrap-stream/buf stream)))

(defun %word-wrap-push-string (stream string start end)
  (let ((s (or start 0))
        (e (or end (length string))))
    (unless (<= s e)
      (error "Start (~a) is greater than end (~a)" start end))
    (loop
      for i from s below e
      do (%word-wrap-push-char stream (aref string i)))))

(defmethod trivial-gray-streams:stream-write-char ((stream word-wrap-stream) char)
  (%word-wrap-push-char stream char))

(defmethod trivial-gray-streams:stream-write-byte ((stream word-wrap-stream) char)
  (error "Can't write binary data to word-wrap streams"))

(defmethod trivial-gray-streams:stream-write-string ((stream word-wrap-stream) string &optional start end)
  (%word-wrap-push-string stream string start end))

(defmethod trivial-gray-streams:stream-write-sequence ((stream word-wrap-stream) string start end &key &allow-other-keys)
  (declare (ignore start end))
  (error "Can't write binary data to word-wrap streams"))

(defmethod trivial-gray-streams:stream-finish-output ((stream word-wrap-stream))
  (%finalise-word-wrap-string stream)
  (call-next-method))

(defmethod trivial-gray-streams:stream-terpri ((stream word-wrap-stream))
  (%word-wrap-push-char stream #\Newline))

(defmethod trivial-gray-streams:stream-line-column ((stream word-wrap-stream))
  (%finalise-word-wrap-string stream)
  (call-next-method))

(defmethod trivial-gray-streams:stream-fresh-line ((stream word-wrap-stream))
  (%finalise-word-wrap-string stream)
  (call-next-method))

(defmethod clim-internals::invoke-with-sheet-medium-bound (continuation medium (sheet word-wrap-stream))
  (%finalise-word-wrap-string sheet)
  (clim-internals::invoke-with-sheet-medium-bound continuation medium (clim:encapsulating-stream-stream sheet)))

(defmacro with-word-wrap ((stream) &body body)
  (let ((stream-sym (gensym))
        (wrapped-stream-sym (gensym)))
    `(let ((,stream-sym ,stream))
       (let ((,wrapped-stream-sym (make-instance 'word-wrap-stream :stream ,stream-sym)))
         (let ((,stream ,wrapped-stream-sym))
           (progn ,@body)
           (%finalise-word-wrap-string ,wrapped-stream-sym))))))

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
  (with-word-wrap (stream)
    (loop
      for v in (foo-frame/content frame)
      for first = t then nil
      unless first
        do (progn
             (finish-output stream)
             (present-horizontal-separator stream))
      do (format stream "~a~%" v))))

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
