(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim")
    (ql:quickload "log4cl")))

(defpackage :clim-test
  (:use :cl))

(in-package :clim-test)

(defparameter *out* *debug-io*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Linebuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass linebuffer-line ()
  ((text          :type flexichain:flexichain
                  :accessor linebuffer-line/text)
   (output-record :type t
                  :initform nil
                  :accessor linebuffer-line/output-record))
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

(defun linebuffer-text-on-line (linebuffer row)
  (linebuffer-line/text (flexichain:element* (linebuffer/lines linebuffer) row)))

(defun process-cursor-movement (linebuffer name)
  (with-accessors ((row linebuffer/row)
                   (col linebuffer/col))
      linebuffer
    (let ((lines (linebuffer/lines linebuffer)))
      (ecase name
        (:left
         (if (zerop col)
             ;; We are at the beginning of the line
             (when (plusp row)
               (setf col (flexichain:nb-elements (linebuffer-text-on-line linebuffer (1- row))))
               (decf row))
             ;; ELSE: In the middle of the line, simply move the cursor
             (decf col)))
        (:right
         (if (= col (flexichain:nb-elements (linebuffer-text-on-line linebuffer row)))
             ;; We are at the end of the line
             (when (< row (1- (flexichain:nb-elements lines)))
               (setf col 0)
               (incf row))
             ;; ELSE: Not at the end of a line
             (incf col)))))))

(defun repaint-linebuffer (linebuffer medium)
  (let ((lines (linebuffer/lines linebuffer)))
    (loop
      with style = (clim:make-text-style :sans-serif :roman nil)
      with cursor-line = (linebuffer/row linebuffer)
      with cursor-col = (linebuffer/col linebuffer)
      with left-margin = 5
      with cursor-margin = 2 ; The number of pixels the cursor should extend beyond the normal size of a character
      for i from 0 below (flexichain:nb-elements lines)
      for line = (flexichain:element* lines i)
      for string = (linebuffer-line-content-as-text line)
      for height = (clim:text-style-height style medium)
      for y = (* height (1+ i))
      for line-start-pos = (clim:make-point left-margin y)
      if (= i cursor-line)
        do (let* ((s (subseq string 0 cursor-col))
                  (cursor-pos (+ (clim:text-size medium s :text-style style) left-margin)))
             (clim:draw-text medium string line-start-pos :text-style style)
             (clim:draw-line* medium
                              cursor-pos
                              (+ y cursor-margin)
                              cursor-pos
                              (- y height cursor-margin)))
      else
        do (clim:draw-text medium string line-start-pos :text-style style))))

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
  (clim:with-sheet-medium (medium pane)
    (clim:draw-rectangle medium (clim:make-point 0 0) (clim:make-point 300 300)
                         :ink (clim:make-rgb-color 1 1 1))
    (repaint-linebuffer (new-edit/linebuffer pane) medium)))

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
    (let ((linebuffer (new-edit/linebuffer pane))
          (ch (clim:keyboard-event-character event))
          (name (clim:keyboard-event-key-name event)))
      (cond ((eq name :return)
             (format *out* "blah~%")
             (linebuffer-insert-newline linebuffer)
             (repaint))
            ((member name '(:up :down :left :right))
             (process-cursor-movement linebuffer name)
             (repaint))
            ((characterp ch)
             (linebuffer-insert-string-at-cursor linebuffer (string ch))
             (repaint))))))

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
