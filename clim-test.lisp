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
    (ql:quickload "mcclim")
    #+nil(ql:quickload "mcclim-gtkairo"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl")))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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
            :initform "foo"
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

(defun display-text-content (frame stream)
  (format stream "~a" (foo-frame/content frame)))

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
