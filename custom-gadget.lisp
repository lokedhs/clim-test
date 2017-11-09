(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim")
    (ql:quickload "receptacle")
    (ql:quickload "log4cl")))

(defpackage :clim-test
  (:use :cl))

(in-package :clim-test)

(defparameter *out* *debug-io*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linebuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass linebuffer-line ()
  ((text :type flexichain:flexichain
         :accessor linebuffer-line/text))
  (:documentation "A single line in a linebuffer"))

(defmethod initialize-instance :after ((obj linebuffer-line) &key text)
  (let ((content (make-instance 'flexichain:standard-cursorchain)))
    (setf (slot-value obj 'text) content)
    (when text
      (loop
        for ch across text
        do (flexichain:push-end content ch)))))

(defun linebuffer-line-content-as-text (line)
  (let ((text (linebuffer-line/text line)))
    (with-output-to-string (s)
      (loop
        for i from 0 below (flexichain:nb-elements text)
        do (write-char (flexichain:element* text i) s)))))

(defclass linebuffer ()
  ((lines :type flexichain:flexichain
          :reader linebuffer/lines)
   (row   :type integer
          :initform 0
          :accessor linebuffer/row)
   (col   :type integer
          :initform 0
          :accessor linebuffer/col))
  (:documentation "Class that holds a list of lines that can be edited"))

(defmethod initialize-instance :after ((obj linebuffer) &key)
  (let ((lines (make-instance 'flexichain:standard-cursorchain)))
    (setf (slot-value obj 'lines) lines)
    (flexichain:insert* lines 0 (make-instance 'linebuffer-line))))

(defun linebuffer-line-at-cursor (linebuffer)
  (flexichain:element* (linebuffer/lines linebuffer) (linebuffer/row linebuffer)))

(defun linebuffer-insert-string-at-cursor (linebuffer string)
  (let* ((line (linebuffer-line-at-cursor linebuffer))
         (cursor (make-instance 'flexichain:right-sticky-flexicursor
                                :chain (linebuffer-line/text line)
                                :position (linebuffer/col linebuffer))))
    (loop
      for ch across string
      do (flexichain:insert cursor ch))
    (setf (linebuffer/col linebuffer) (flexichain:cursor-pos cursor))))

(defun linebuffer-insert-newline (buffer)
  (with-accessors ((lines linebuffer/lines)
                   (row linebuffer/row)
                   (col linebuffer/col))
      buffer
    (let* ((lines (linebuffer/lines buffer))
           (line (flexichain:element* lines row))
           (text (linebuffer-line/text line))
           (text-length (flexichain:nb-elements text))
           (new-row (make-instance 'linebuffer-line))
           (new-row-index (1+ row)))
      (when (> col text-length)
        (loop
          for i from col below text-length
          do (flexichain:push-end (linebuffer-line/text new-row) (flexichain:element* line i)))
        (flexichain:delete-elements* line col (- text-length col)))
      (flexichain:insert* lines new-row-index new-row)
      (setf col 0)
      (setf row new-row-index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test gadget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass new-edit (clim:text-editor)
  ((linebuffer :type t
               :initform (make-instance 'linebuffer)
               :reader new-edit/linebuffer))
  (:documentation "New implementation of text editor"))

(defmethod clim:compose-space ((pane new-edit) &key width height)
  (declare (ignore width height))
  (format *out* "compose space~%")
  (clim:make-space-requirement :width 300 :height 200))

(defmethod clim:handle-repaint ((pane new-edit) region)
  (format *out* "Repaint here~%")
  (let ((content (linebuffer/lines (new-edit/linebuffer pane))))
    (clim:with-sheet-medium (medium pane)
      (clim:draw-rectangle medium (clim:make-point 0 0) (clim:make-point 300 300)
                           :ink (clim:make-rgb-color 1 1 1))
      (loop
        for i from 0 below (flexichain:nb-elements content)
        for line = (flexichain:element* content i)
        do (clim:draw-text medium (linebuffer-line-content-as-text line) (clim:make-point 30 (* 10 (1+ i))))))))

(defmethod clim:handle-event ((pane new-edit) (event clim:pointer-button-event))
  (format *out* "Button click: ~a,~a~%"
          (clim:pointer-event-native-x event)
          (clim:pointer-event-native-y event))
  (setf (clim:port-keyboard-input-focus (clim:find-port)) pane))

(defmethod clim:handle-event ((pane new-edit) (event clim:key-press-event))
  (format *out* "Keypress: name: ~s, char: ~s~%"
          (clim:keyboard-event-key-name event)
          (clim:keyboard-event-character event))
  (labels ((repaint ()
             (clim:repaint-sheet pane (clim:make-rectangle (clim:make-point 0 0) (clim:make-point 300 300)))))
    (let ((ch (clim:keyboard-event-character event)))
      (case (clim:keyboard-event-key-name event)
        (:return
          (format *out* "blah~%")
          (linebuffer-insert-newline (new-edit/linebuffer pane))
          (repaint))
        (t
         (when (characterp ch)
           (linebuffer-insert-string-at-cursor (new-edit/linebuffer pane) (string ch))
           (repaint)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *editor-pane-border* (clim:make-rgb-color 0 0 0))

  (defclass post-content ()
    ())

  (defclass post-text-record (climi::accepting-values-record)
    ((editor :accessor post-text-record/editor))))

(defmethod climi::select-query (stream query (record post-text-record))
  )

(defmethod climi::deselect-query (stream query (record post-text-record))
  )

(defmethod climi::finalize-query-record (query (record post-text-record))
  (let ((editor (post-text-record/editor record)))
    (when editor
      (setf (climi::value query) (clim:gadget-value editor)))))

(defvar *editor* nil)

(defun make-test-gadget ()
  (clim:make-pane 'new-edit))

(clim:define-presentation-method clim:accept-present-default ((type post-content)
                                                              stream (view t)
							      default default-supplied-p present-p query-identifier)
  (let* ((editor nil)
	 (record (clim:updating-output (stream :unique-id query-identifier
                                               :cache-value nil
		                               :record-type 'post-text-record)
                   (clim:surrounding-output-with-border (stream :ink *editor-pane-border* :padding 2)
	            (clim:with-output-as-gadget (stream)
	              (setq editor (make-test-gadget)))))))
    (when editor
      (setq *editor* editor)
      (setf (post-text-record/editor record) editor))
    record))

#+nil
(clim:define-presentation-method clim:accept ((type post-content) stream (view t) &key)
  (clim:with-output-as-gadget (stream)
    (clim:make-pane 'clim:text-editor-pane :ncolumns 40 :nlines 8)))

(defun accepting-post (&key (stream *query-io*) (own-window nil))
  (let (content)
    (clim:accepting-values (stream :resynchronize-every-pass t :own-window own-window)
      (setq content (clim:accept 'post-content :prompt "Post Content" :stream stream :default "some text"))
      (fresh-line stream))
    content))

(defun display-description (frame stream)
  (declare (ignore frame))
  (format stream "Test program for accept"))

(clim:define-application-frame mastodon-frame ()
  ()
  (:panes (description-pane :application
                            :display-function 'display-description)
          (interaction-pane :interactor))
  (:layouts (default description-pane
                     interaction-pane)))

(define-mastodon-frame-command (post :name "Post")
    ()
  (let ((content (accepting-post)))
    (format *out* "Got content: ~s~%" content)))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'mastodon-frame
                                            :width 800 :height 700)))
    (clim:run-frame-top-level frame)))
