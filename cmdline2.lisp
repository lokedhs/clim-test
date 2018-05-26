(defpackage :cmdline2-test
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :cmdline2-test)

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
          (clim:accept 'string :stream stream :prompt nil :default "" :default-type 'string)))
    (log:info "Got input: object=~s, type=~s" object type)
    (let ((expression (string-trim " " object)))
      (unless (equal expression "")
        `(maxima-eval ,expression)))))

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

(defvar *font-roman* '("Latin Modern Math" "Regular"))
(defvar *font-italic* '("Noto Serif" "Italic"))

(defvar *font-size*)
(defvar *rop*)
(defvar *lop*)

(defparameter *invert-readtable* (let ((readtable (copy-readtable)))
                                   (setf (readtable-case readtable) :invert)
                                   readtable))

(defclass rendered-expr ()
  ((output-record :initarg :output-record
                  :reader rendered-expr/output-record)))

(defmacro with-output-to-rendered-expr ((stream) &body body)
  (alexandria:once-only (stream)
    (alexandria:with-gensyms (output-record)
      `(let ((,output-record (clim:with-output-to-output-record (,stream)
                               ,@body)))
         (make-instance 'rendered-expr :output-record ,output-record)))))

(defmacro iterate-exprs ((sym exprs op &key first-sym) &body body)
  (alexandria:with-gensyms (run-body p v first)
    (alexandria:once-only (exprs op)
      `(labels ((,run-body (,p ,first)
                  (let ((,sym ,p))
                    ,(if first-sym
                         `(let ((,first-sym ,first))
                            ,@body)
                         `(progn ,@body)))))
         (when ,exprs
           (cond ((and (car ,exprs)
                       (null (cdr ,exprs)))
                  (,run-body (car ,exprs) t))
                 (t
                  (let ((*rop* ,op))
                    (,run-body (car ,exprs) t))
                  (let ((*lop* ,op))
                    (loop
                      for ,v on (cdr ,exprs)
                      if (cdr ,v)
                        do (let ((*rop* ,op))
                             (,run-body (car ,v) nil))
                      else
                        do (,run-body (car ,v) nil))))))))))

(defmacro with-paren-op (&body body)
  `(let ((*lop* 'maxima::mparen)
         (*rop* 'maxima::mparen))
     ,@body))

(defun render-formatted (stream fmt &rest args)
  (with-output-to-rendered-expr (stream)
    (clim:draw-text* stream (apply #'format nil fmt args) 0 0)))

(defun render-number (stream n)
  (render-formatted stream "~a" n))

(defun render-symbol (stream sym)
  (case sym
    (maxima::$inf (render-formatted stream "~c" #\INFINITY))
    (maxima::$%pi (render-formatted stream "~c" #\GREEK_SMALL_LETTER_PI))
    (t (let ((n (let ((*readtable* *invert-readtable*)) (princ-to-string sym))))
         (if (eql (aref n 0) #\$)
             (clim:with-text-style (stream (clim-internals::make-text-style (first *font-italic*)
                                                                            (second *font-italic*)
                                                                            *font-size*))
               (render-formatted stream "~a" (subseq n 1)))
             (render-formatted stream "~s" sym))))))

(defun render-plus (stream exprs)
  (iterate-exprs (expr exprs 'maxima::mplus :first-sym first)
    ))

(defun render-maxima-expression (stream expr)
  (labels ((render-inner (fixed)
             (ecase (caar fixed)
               (maxima::mplus (render-plus stream (cdr fixed))))))
    (let ((fixed (maxima::nformat-check expr)))
      (log:info "Calling render expression on: ~s" fixed)
      (etypecase fixed
        (number (render-number stream fixed))
        (symbol (render-symbol stream fixed))
        (list (if (or (<= (maxima::lbp (caar fixed)) (maxima::rbp *lop*))
                      (<= (maxima::rbp (caar fixed)) (maxima::lbp *rop*)))
                  (wrap-with-parens stream (render-inner fixed))
                  (render-inner fixed)))))))

(defun make-expression-output-record (stream expr)
  (log:info "Making output record for expr: ~s" expr)
  (let ((*font-size* 14))
    (clim:with-text-style (stream (clim-internals::make-text-style (first *font-roman*)
                                                                   (second *font-roman*)
                                                                   *font-size*))
      (let ((rendered-expr (with-paren-op
                             (render-maxima-expression stream expr))))
        (setf (clim:output-record-position (rendered-expr/output-record rendered-expr)) (values 0 0))
        (rendered-expr/output-record rendered-expr)))))
