(defpackage :clim-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :clim-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    #+nil(dolist (p (mapcar #'pathname-directory (directory #p"/home/emartenson/prog/McCLIM-gtk/**/*.asd")))
           (pushnew (make-pathname :directory p) asdf:*central-registry* :test #'equal))
    #+nil
    (dolist (p (mapcar #'pathname-directory (directory #p"/home/emartenson/src/McCLIM-xkb/**/*.asd")))
      (pushnew (make-pathname :directory p) asdf:*central-registry* :test #'equal))
    (unless (find-package "LOG4CL")
      (ql:quickload "log4cl"))
    (ql:quickload "mcclim")
    #+nil(ql:quickload "mcclim-gtkairo"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun present-to-stream (obj stream)
  (clim:present obj (clim:presentation-type-of obj) :stream stream))

(defclass update-application-pane (clim:application-pane)
  ())

(defvar *in-redraw* nil)

(defmethod clim:note-sheet-region-changed :after ((sheet update-application-pane))
  (unless *in-redraw*
    (let ((*in-redraw* t))
      (clim:redisplay-frame-pane (clim:pane-frame sheet) sheet))))

(defclass text-content-view (clim:view)
  ())

(clim:define-application-frame foo-frame ()
  ((content :type string
            :initarg :content
            :initform ""
            :accessor foo-frame/content))
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content
                        :redisplay-on-resize-p t)
          (interaction-pane :interactor))
  (:layouts (default (clim:vertically ()
                       text-content
                       interaction-pane))))

#+nil
(defmethod clim:note-sheet-region-changed :after ((sheet clim:standard-output-recording-stream))
  (log:trace "CHANGE sheet = ~s" sheet)
  (if *in-redraw*
      (log:info "In redraw, not redisplaying")
      (let ((*in-redraw* t))
        (clim:redisplay-frame-pane (clim:pane-frame sheet) sheet)))
  #+nil(clim:redisplay-frame-pane (clim:pane-frame sheet) sheet)
  #+nil(setf (clim:pane-needs-redisplay sheet) t))

#+nil(defmethod clim:note-sheet-region-changed :around ((sheet t))
  (log:trace "CHANGE sheet = ~s" sheet)
  (prog1
      (call-next-method)
    (when (eq (clim:pane-name sheet) 'text-content)
      (clim:redisplay-frame-pane (clim:pane-frame sheet) sheet))))

#+nil(defmethod clim:note-sheet-transformation-changed :around ((sheet t))
  (log:trace "TRANSFORM sheet = ~s" sheet)
  (call-next-method))

(define-foo-frame-command (add-text :name "Add text")
    ((text 'string))
  (with-accessors ((content foo-frame/content)) clim:*application-frame*
    (setf content (concatenate 'string content " " text))))

(define-foo-frame-command (add-complex-chars :name "Add Unicode")
    ()
  (with-accessors ((content foo-frame/content)) clim:*application-frame*
    (setf content (format nil "~a Snowman: ~c, Precomposed: ~c, Composed: ~c~c, Supplementary: ~c, Cypriot: ~c"
                          content
                          #\SNOWMAN
                          #\LATIN_SMALL_LETTER_A_WITH_DIAERESIS
                          #\a #\COMBINING_DIAERESIS
                          #\KISS_MARK
                          #\CYPRIOT_SYLLABLE_NA))))

(define-foo-frame-command (add-random-text :name "Add random text")
    ((count 'integer))
  (add-text (with-output-to-string (s)
              (loop
                for i from 0 below (or count 200)
                if (and (not (zerop i)) (zerop (random 8)))
                  do (write-char #\Space s)
                do (write-char (code-char (+ (char-code #\a) (random (1+ (- (char-code #\z) (char-code #\a)))))) s)))))

(define-foo-frame-command (show-pane :name "Show pane")
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'text-content)))
    (log:info "Pane: ~s" pane)
    (break)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass post-content ()
    ()))

(clim:define-presentation-method clim:accept ((type post-content) stream view &key)
  (format stream "Foo"))

(define-foo-frame-command (accept-test :name "Accept Test")
    ()
  (clim:accepting-values ()
    (clim:accept 'string :prompt "Some string")
    (clim:accept 'post-content :prompt "Post content")))

(defun draw-paren-test (stream)
  (let ((output-record (clim:with-output-to-output-record (stream)
                         (format stream "foo")))
        (left-paren (clim:with-output-to-output-record (stream)
                      (clim:with-text-size (stream 30)
                        (format stream "("))))
        (right-paren (clim:with-output-to-output-record (stream)
                       (clim:with-text-size (stream 30)
                         (format stream ")")))))
    (log:info "baselines: o:~f, l:~f, r:~f"
              (clim-extensions:output-record-baseline output-record)
              (clim-extensions:output-record-baseline left-paren)
              (clim-extensions:output-record-baseline right-paren))
    (multiple-value-bind (width height)
        (clim:rectangle-size output-record)
      (multiple-value-bind (left-paren-width left-paren-height)
          (clim:rectangle-size left-paren)
        (multiple-value-bind (right-paren-width right-paren-height)
            (clim:rectangle-size right-paren)
          ;;
          (log:info "heights: o:~f, l:~f, r:~f"
                    height left-paren-height right-paren-height)
          (log:info "positions: o:~s, l:~s, r:~s"
                    (multiple-value-list (clim:output-record-position output-record))
                    (multiple-value-list (clim:output-record-position left-paren))
                    (multiple-value-list (clim:output-record-position right-paren)))
          ;;
          (setf (clim:output-record-position left-paren)
                (values 0
                        (- (clim-extensions:output-record-baseline left-paren))))
          (clim:stream-add-output-record stream left-paren)
          ;;
          (setf (clim:output-record-position output-record)
                (values left-paren-width
                        (- (clim-extensions:output-record-baseline output-record))))
          (clim:stream-add-output-record stream output-record)
          ;;
          (setf (clim:output-record-position right-paren)
                (values (+ left-paren-width width)
                        (- (clim-extensions:output-record-baseline right-paren))))
          (clim:stream-add-output-record stream right-paren)
          ;;
          (clim:draw-text* stream "xyz" (+ width left-paren-width right-paren-width) 0)
          ;;
          (clim:draw-line* stream 0 0 100 50)
          ;;
          (log:info "positions: o:~s, l:~s, r:~s"
                    (mapcar #'float (multiple-value-list (clim:output-record-position output-record)))
                    (mapcar #'float (multiple-value-list (clim:output-record-position left-paren)))
                    (mapcar #'float (multiple-value-list (clim:output-record-position right-paren)))))))))

(defun display-text-content (frame stream)
  (clim:with-text-style (stream (clim-internals::make-text-style "Noto Serif" "Regular" 24))
    #+nil
    (let ((rec (clim:with-output-to-output-record (stream)
                 (let ((fraction-spacing 2)
                       (top (clim:with-output-to-output-record (stream)
                              (format stream "top with more text")))
                       (bottom (clim:with-output-to-output-record (stream)
                                 (format stream "bottom"))))
                   (multiple-value-bind (top-width top-height)
                       (clim:rectangle-size top)
                     (multiple-value-bind (bottom-width bottom-height)
                         (clim:rectangle-size bottom)
                       (declare (ignore bottom-height))
                       (let ((max-width (max top-width)))
                         (setf (clim:output-record-position top)
                               (values (/ (- max-width top-width) 2) 0))
                         (setf (clim:output-record-position bottom)
                               (values (/ (- max-width bottom-width) 2) (+ top-height (+ (* fraction-spacing 2) 1))))
                         (clim:stream-add-output-record stream top)
                         (clim:stream-add-output-record stream bottom)
                         (let ((y (+ top-height fraction-spacing)))
                           (clim:draw-line* stream 0 y max-width y)))))))))
      (clim:with-room-for-graphics (stream)
        (clim:stream-add-output-record stream rec)))
    ;;
    #+nil (clim:draw-line* stream 0 0 200 100)
    (let ((rec (clim:with-output-to-output-record (stream)
                 (draw-paren-test stream))))
      (multiple-value-bind (width)
          (clim:rectangle-size rec)
        (setf (clim:output-record-position rec)
              (values 0 0))
        (clim:stream-add-output-record stream rec)
        (log:info "baseline: ~s" (clim-extensions:output-record-baseline rec))
        (clim:draw-text* stream "Should have the same baseline" width (clim-extensions:output-record-baseline rec))))
    ;;
    (format stream "~a" (foo-frame/content frame))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame)))
    (clim:run-frame-top-level frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Post form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass post-content ()
  ())

(defparameter *editor-pane-border* (clim:make-rgb-color 0 0 0))

(defclass post-text-record (climi::accepting-values-record)
  ((editor :accessor post-text-record/editor)))

(defmethod climi::select-query (stream query (record post-text-record))
  )

(defmethod climi::deselect-query (stream query (record post-text-record))
  )

(defmethod climi::finalize-query-record (query (record post-text-record))
  (let ((editor (post-text-record/editor record)))
    (when editor
      (setf (climi::value query) (clim:gadget-value editor)))))

(clim:define-presentation-method clim:accept-present-default ((type post-content)
                                                              stream (view t)
							      default default-supplied-p present-p query-identifier)
  (let* ((editor nil)
	 (record (clim:updating-output (stream :unique-id query-identifier
                                               :cache-value (if default-supplied-p default "")
		                               :record-type 'post-text-record)
                   (clim:surrounding-output-with-border (stream :ink *editor-pane-border* :padding 2)
	            (clim:with-output-as-gadget (stream)
	              (setq editor (clim:make-pane 'clim:text-editor :ncolumns 10 :nlines 8)))))))
    (when editor 
      (setf (post-text-record/editor record) editor))
    record))

(defun accepting-post (&key (stream *query-io*) (own-window nil))
  (let (content)
    (clim:accepting-values (stream :resynchronize-every-pass t :own-window own-window)
      (setq content (clim:accept 'post-content :prompt "Post Content" :stream stream)))
    content))

(define-foo-frame-command (post :name "Post")
    ()
  (let ((text (accepting-post)))
    (format *debug-io* "Text: ~s~%" text)))
