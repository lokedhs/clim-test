(defpackage :cmdline-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :cmdline-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (push #p"~/src/maxima-code/src/" asdf:*central-registry*)
  (push #p"~/src/maxima-code/share/odepack/" asdf:*central-registry*)
  (ql:quickload "maxima"))

(clim:define-command-table maxima-commands)

(defclass maxima-interactor-view (clim:textual-view)
  ())

(defparameter +listener-view+ (make-instance 'maxima-interactor-view))

(defclass maxima-interactor-pane (clim:interactor-pane)
  ())

(clim:define-application-frame maxima-main-frame ()
  ()
  (:panes (text-content (clim:make-clim-stream-pane :type 'maxima-interactor-pane
                                                    :name 'maxima-interactor
                                                    :default-view +listener-view+)))
  (:top-level (clim:default-frame-top-level :prompt 'print-listener-prompt))
  (:command-table (maxima-main-frame :inherit-from (maxima-commands)))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defmethod clim:read-frame-command ((frame maxima-main-frame) &key (stream *standard-input*))
  (multiple-value-bind (object type)
      (let ((clim:*command-dispatchers* '(#\;)))
        (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
          (clim:accept 'string :stream stream :prompt nil :default "foo" :default-type 'string)))
    (log:info "Got input: object=~s, type=~s" object type)
    `(maxima-eval ,object)))

(defmethod clim:stream-present :around ((stream maxima-interactor-pane) object type
                                   &rest args
                                   &key (single-box nil single-box-p) &allow-other-keys)
  (declare (ignore single-box single-box-p))
  (apply #'call-next-method stream object type :single-box t args))

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (format stream "maxima> "))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'maxima-main-frame)))
    (clim:run-frame-top-level frame)))

(clim:define-command (maxima-eval :name "Run command" :menu t :command-table maxima-commands)
    ((cmd 'string :prompt "command"))
  (log:info "eval command. form=~s" cmd)
  (let* ((form (with-input-from-string (s (format nil "~a;~%" cmd))
                 (maxima::dbm-read s nil nil))))
    (assert (and (listp form)
                 (= (length form) 3)
                 (equal (first form) '(maxima::displayinput))
                 (null (second form))))
    (let* ((result (maxima::toplevel-macsyma-eval (third form)))
           (output-record (make-expression-output-record *standard-output* result)))
      (clim:with-room-for-graphics (*standard-output*)
        (clim:surrounding-output-with-border (*standard-output* :padding 10 :ink clim:+transparent-ink+)
          (clim:stream-add-output-record *standard-output* output-record))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maths rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-quotient (stream top-expr bottom-expr)
  (let ((fraction-spacing 2)
        (top (clim:with-output-to-output-record (stream)
               (render-maxima-expression stream top-expr)))
        (bottom (clim:with-output-to-output-record (stream)
                  (render-maxima-expression stream bottom-expr))))
    (multiple-value-bind (top-width top-height)
        (clim:rectangle-size top)
      (multiple-value-bind (bottom-width bottom-height)
          (clim:rectangle-size bottom)
        (declare (ignore bottom-height))
        (let ((max-width (max top-width bottom-width)))
          (setf (clim:output-record-position top)
                (values (/ (- max-width top-width) 2) 0))
          (setf (clim:output-record-position bottom)
                (values (/ (- max-width bottom-width) 2) (+ top-height (+ (* fraction-spacing 2) 1))))
          (clim:stream-add-output-record stream top)
          (clim:stream-add-output-record stream bottom)
          (let ((y (+ top-height fraction-spacing)))
            (clim:draw-line* stream 0 y max-width y)))))))

(defun render-symbol (stream sym)
  (case sym
    (maxima::$inf (format stream "~c" #\INFINITY))
    (maxima::$%pi (format stream "~c" #\GREEK_SMALL_LETTER_PI))
    (t (let ((n (symbol-name sym)))
         (if (eql (aref n 0) #\$)
             (clim:with-text-style (stream (clim:make-text-style :serif :italic :normal))
               (format stream "~a" (string-downcase (subseq n 1))))
             (format stream "~s" sym))))))

(defun render-negation (stream expr)
  (format stream "-")
  (render-maxima-expression stream expr))

(defun %render-and-move (stream pos fn)
  (let ((output-record (clim:with-output-to-output-record (stream)
                         (funcall fn))))
    (multiple-value-bind (w h)
        (clim:rectangle-size output-record)
      (setf (clim:output-record-position output-record) (values pos (- (/ h 2))))
      (clim:stream-add-output-record stream output-record)
      (+ pos w))))

(defmacro render-and-move (stream pos &body body)
  `(%render-and-move ,stream ,pos (lambda () ,@body)))

(defun render-plus (stream exprs)
  (log:info "render plus: ~s" exprs)
  (loop
    with pos = 0
    for expr in exprs
    for first = t then nil
    if (and (listp expr)
            (alexandria:length= (length expr) 2)
            (listp (car expr))
            (eq (caar expr) 'maxima::mminus))
      do (setf pos (render-and-move stream pos (render-negation stream (second expr))))
    else
      do (progn
           (unless first
             (setf pos (render-and-move stream pos (format stream "+"))))
           (setf pos (render-and-move stream pos (render-maxima-expression stream expr))))))

(defun render-times (stream exprs)
  (loop
    for expr in exprs
    for first = t then nil
    unless first
      do (format stream "~c" #\MIDDLE_DOT)
    do (render-maxima-expression stream expr)))

(defun render-expt (stream a b)
  (let ((base (clim:with-output-to-output-record (stream)
                (render-maxima-expression stream a)))
        (exp (clim:with-output-to-output-record (stream)
               (render-maxima-expression stream b))))
    (multiple-value-bind (base-width base-height)
        (clim:rectangle-size base)
      (multiple-value-bind (exp-width exp-height)
          (clim:rectangle-size exp)
        (declare (ignore exp-width))
        (clim:stream-add-output-record stream base)
        (setf (clim:output-record-position exp)
              (values base-width (- (/ base-height 2) exp-height)))
        (clim:stream-add-output-record stream exp)))))

(defun render-plain (stream spacing ch a b)
  (let ((pos 0))
    (setf pos (render-and-move stream pos (render-maxima-expression stream a)))
    (incf pos spacing)
    (setf pos (render-and-move stream pos (format stream "~c" ch)))
    (incf pos spacing)
    (render-and-move stream pos (render-maxima-expression stream b))))

(defun render-equal (stream a b)
  (render-plain stream 4 #\= a b))

(defun render-function (stream name exprs)
  (log:info "Rendering function: name=~s, exprs=~s" name exprs)
  (let ((pos 0))
    (setf pos (render-and-move stream pos (render-symbol stream (car name))))
    (setf pos (render-and-move stream pos (format stream "(")))
    (loop
      for expr in exprs
      for first = t then nil
      unless first
        do (setf pos (render-and-move stream pos (format stream ", ")))
      do (setf pos (render-and-move stream pos (render-maxima-expression stream expr))))
    (render-and-move stream pos (format stream ")"))))

(defun render-maxima-expression (stream expr)
  (let ((fixed (maxima::nformat-check expr)))
    (log:info "Calling render expression on: ~s" fixed)
    (etypecase fixed
      (number (format stream "~a" fixed))
      (symbol (render-symbol stream fixed))
      (list (case (caar fixed)
              (maxima::mquotient (render-quotient stream (second fixed) (third fixed)))
              (maxima::mplus (render-plus stream (cdr fixed)))
              (maxima::mminus (render-negation stream (cdr fixed)))
              (maxima::mtimes (render-times stream (cdr fixed)))
              (maxima::mexpt (render-expt stream (second fixed) (third fixed)))
              (maxima::mequal (render-equal stream (second fixed) (third fixed)))
              (t (render-function stream (car fixed) (cdr fixed))))))))

(defun make-expression-output-record (stream expr)
  (log:info "Making output record for expr: ~s" expr)
  (clim:with-output-to-output-record (stream)
    (render-maxima-expression stream expr)))
