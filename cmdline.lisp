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
(defvar *draw-boxes* nil)

(defvar *aligned-rendering-pos*)
(defvar *aligned-rendering-stream*)
(defvar *font-size*)
(defvar *rop*)
(defvar *lop*)

(defparameter *invert-readtable* (let ((readtable (copy-readtable)))
                                   (setf (readtable-case readtable) :invert)
                                   readtable))

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

(defun set-rec-position (output-record x y)
  (dimension-bind (output-record :x old-x :y old-y)
    (setf (clim:output-record-position output-record)
          (values (or x old-x)
                  (or y old-y)))))

(defun move-rec (output-record dx dy)
  (dimension-bind (output-record :x old-x :y old-y)
    (setf (clim:output-record-position output-record)
          (values (+ dx old-x)
                  (+ dy old-y)))))

(defun make-boxed-output-record (stream rec)
  (if *draw-boxes*
      (clim:with-output-to-output-record (stream)
        (dimension-bind (rec :x x :y y :right right :bottom bottom)
          (clim:stream-add-output-record stream rec)
          (clim:draw-rectangle* stream x y (1- right) (1- bottom) :filled nil)))
      rec))

(defmacro with-roman-text-style ((stream) &body body)
  `(clim:with-text-style (,stream (clim:make-text-style (first *font-roman*)
                                                        (second *font-roman*)
                                                        *font-size*))
     ,@body))

(defmacro with-italic-text-style ((stream) &body body)
  `(clim:with-text-style (,stream (clim:make-text-style (first *font-italic*)
                                                        (second *font-italic*)
                                                        *font-size*))
     ,@body))

(defmacro with-font-size ((stream size) &body body)
  (alexandria:once-only (stream size)
    `(let ((*font-size* ,size))
       (clim:with-text-size (,stream *font-size*)
         ,@body))))

(defmacro with-font-size-change ((stream mod) &body body)
  (alexandria:once-only (stream mod)
    `(let ((*font-size* (max (* *font-size* ,mod) 10)))
       (clim:with-text-size (,stream *font-size*)
         ,@body))))

(defun %aligned-render-and-move (stream pos fn)
  (let ((output-record (clim:with-output-to-output-record (stream)
                         (funcall fn))))
    (multiple-value-bind (w)
        (clim:rectangle-size output-record)
      (move-rec output-record pos 0)
      (clim:stream-add-output-record stream output-record)
      (+ pos w))))

(defmacro with-aligned-rendering ((stream) &body body)
  `(let ((*aligned-rendering-pos* 0)
         (*aligned-rendering-stream* ,stream))
     ,@body))

(defmacro render-aligned (() &body body)
  `(setf *aligned-rendering-pos* (%aligned-render-and-move *aligned-rendering-stream* *aligned-rendering-pos*
                                                           (lambda () ,@body))))

(defun render-aligned-string (fmt &rest args)
  (render-aligned ()
    (clim:draw-text* *aligned-rendering-stream* (apply #'format nil fmt args) 0 0)))

(defun render-formatted (stream fmt &rest args)
  (with-aligned-rendering (stream)
    (apply #'render-aligned-string fmt args)))

(defmacro with-paren-op (&body body)
  `(let ((*lop* 'maxima::mparen)
         (*rop* 'maxima::mparen))
     ,@body))

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

(defun char-height (stream)
  (multiple-value-bind (width height)
      (clim:text-size stream "A")
    (declare (ignore width))
    height))

(defun char-descent (stream char)
  (multiple-value-bind (width height x y baseline)
      (clim:text-size stream (format nil "~c" char))
    (declare (ignore width x y))
    (- height baseline)))

(defun render-quotient (stream top-expr bottom-expr)
  (let ((fraction-spacing 2)
        (top (clim:with-output-to-output-record (stream)
               (render-maxima-expression stream top-expr)))
        (bottom (clim:with-output-to-output-record (stream)
                  (render-maxima-expression stream bottom-expr))))
    (dimension-bind (top :width top-width :height top-height)
      (dimension-bind (bottom :width bottom-width)
        (let* ((max-width (max top-width bottom-width))
               (y (+ top-height fraction-spacing))
               (centre (+ y (/ (char-height stream) 2))))
          (set-rec-position top
                            (/ (- max-width top-width) 2)
                            (- centre))
          (clim:stream-add-output-record stream top)
          (set-rec-position bottom
                            (/ (- max-width bottom-width) 2)
                            (- (+ top-height (+ (* fraction-spacing 2) 1)) centre))
          (clim:stream-add-output-record stream bottom)
          (clim:draw-line* stream 0 (- y centre) max-width (- y centre)))))))

(defun render-symbol (stream sym)
  (case sym
    (maxima::$inf (render-formatted stream "~c" #\INFINITY))
    (maxima::$%pi (render-formatted stream "~c" #\GREEK_SMALL_LETTER_PI))
    (t (let ((n (let ((*readtable* *invert-readtable*)) (princ-to-string sym))))
         (if (eql (aref n 0) #\$)
             (with-italic-text-style (stream)
               (render-formatted stream "~a" (subseq n 1)))
             (render-formatted stream "~s" sym))))))

(defun render-negation (stream expr)
  (log:info "Render negation: ~s" expr)
  (with-aligned-rendering (stream)
    (render-aligned-string "-")
    (let ((*lop* 'maxima::mminus))
      (render-aligned () (render-maxima-expression stream expr)))))

(defun render-plus (stream exprs)
  (log:info "render plus: ~s" exprs)
  (with-aligned-rendering (stream)
    (iterate-exprs (expr exprs 'maxima::mplus :first-sym first)
      (cond ((and (listp expr)
                  (alexandria:length= (length expr) 2)
                  (listp (car expr))
                  (eq (caar expr) 'maxima::mminus))
             (render-aligned () (render-negation stream (second expr))))
            (t
             (unless first
               (render-aligned-string "+"))
             (render-aligned () (render-maxima-expression stream expr)))))))

(defun render-times (stream exprs)
  (with-aligned-rendering (stream)
    (iterate-exprs (expr exprs 'maxima::mtimes :first-sym first)
      (unless first
        (render-aligned-string "~c" #\MIDDLE_DOT))
      (render-aligned () (render-maxima-expression stream expr)))))

(defun render-expt (stream a b)
  (let ((base (clim:with-output-to-output-record (stream)
                (let ((*rop* 'maxima::mexpt))
                 (render-maxima-expression stream a))))
        (exp (clim:with-output-to-output-record (stream)
               (with-font-size-change (stream 2/3)
                 (let ((*lop* 'maxima::mexpt))
                   (render-maxima-expression stream b))))))
    (dimension-bind (base :height base-height :y base-y :right base-right)
      (dimension-bind (exp :height exp-height)
        (clim:stream-add-output-record stream (make-boxed-output-record stream base))
        (set-rec-position exp base-right
                          (if (>= exp-height (/ base-height 2))
                              ;; exp is high enough that it needs to have the bottom aligned to
                              ;; the middle of base
                              (- (+ base-y (/ base-height 2)) exp-height)
                              ;; ELSE: the top of exp should be slightly above the top of base
                              ;; For now, just align them
                              base-y))
        (clim:stream-add-output-record stream (make-boxed-output-record stream exp))))))

(defun render-plain (stream spacing ch a b)
  (with-aligned-rendering (stream)
    (render-aligned () (render-maxima-expression stream a))
    (incf *aligned-rendering-pos* spacing)
    (render-aligned-string "~c" ch)
    (incf *aligned-rendering-pos* spacing)
    (render-aligned () (render-maxima-expression stream b))))

(defun render-equal (stream a b)
  (render-plain stream 4 #\= a b))

(defun wrap-with-parens (stream output-record)
  (dimension-bind (output-record :y y :width width :height height)
    (clim:rectangle-size output-record)
    (let ((left-paren (clim:with-output-to-output-record (stream)
                        (clim:with-text-size (stream height)
                          (render-formatted stream "("))))
          (right-paren (clim:with-output-to-output-record (stream)
                         (clim:with-text-size (stream height)
                           (render-formatted stream ")")))))
      (let ((pos y))
        (multiple-value-bind (left-paren-width)
            (clim:rectangle-size left-paren)
          (set-rec-position left-paren 0 pos)
          (clim:stream-add-output-record stream left-paren)
          ;;
          (move-rec output-record left-paren-width 0)
          (clim:stream-add-output-record stream output-record)
          ;;
          (set-rec-position right-paren (+ left-paren-width width) pos)
          (clim:stream-add-output-record stream right-paren))))))

(defun render-function (stream name exprs)
  (with-aligned-rendering (stream)
    (render-aligned () (render-symbol stream (car name)))
    (let ((params (clim:with-output-to-output-record (stream)
                    (with-aligned-rendering (stream)
                      (loop
                        for expr in exprs
                        for first = t then nil
                        unless first
                          do (render-aligned-string ", ")
                        do (render-aligned () (render-maxima-expression stream expr)))))))
      (render-aligned () (wrap-with-parens stream params)))))

(defun render-and-measure-string (stream string &optional (x 0) (y 0))
  (multiple-value-bind (width height final-x final-y baseline)
      (clim:text-size stream string)
    (declare (ignore width final-x final-y))
    (let ((rec (clim:with-output-to-output-record (stream)
                 (clim:draw-text* stream string x y))))
      (list rec baseline (- height baseline)))))

(defun render-intsum-inner (stream f var from to symbol sym2)
  (let* ((bottom (clim:with-output-to-output-record (stream)
                   (with-aligned-rendering (stream)
                     (with-paren-op
                       (when var
                         (render-aligned () (render-maxima-expression stream var))
                         (render-aligned () (render-formatted stream "=")))
                       (render-aligned () (render-maxima-expression stream from))))))
         (top    (clim:with-output-to-output-record (stream)
                   (with-paren-op
                     (render-maxima-expression stream to))))
         (exp    (clim:with-output-to-output-record (stream)
                   (let ((*lop* 'maxima::%sum))
                     (render-maxima-expression stream f)))))
    (dimension-bind (exp :height exp-height)
      (destructuring-bind (sigma sigma-ascent sigma-descent)
          (clim:with-text-size (stream (+ 10 exp-height))
            (render-and-measure-string stream (format nil "~c" symbol)))
        (dimension-bind (sigma :width sigma-width :right sigma-right :x sigma-x)
          (let ((sigma-top (- sigma-ascent))
                (sigma-bottom sigma-descent)
                (sigma-height (+ sigma-ascent sigma-descent))
                (centre (- (/ (+ sigma-ascent sigma-descent) 2) (/ (char-height stream) 2))))
            (move-rec sigma 0 centre)
            (clim:stream-add-output-record stream (make-boxed-output-record stream sigma))
            ;;
            (dimension-bind (bottom :width bottom-width)
              (set-rec-position bottom
                                (+ sigma-x (/ (- sigma-width bottom-width) 2))
                                (+ sigma-bottom centre))
              (clim:stream-add-output-record stream (make-boxed-output-record stream bottom)))
            ;;
            (dimension-bind (top :width top-width :height top-height)
              (set-rec-position top
                                (/ (- sigma-width top-width) 2)
                                (+ (- sigma-top top-height) centre))
              (clim:stream-add-output-record stream (make-boxed-output-record stream top)))
            ;;
            (dimension-bind (exp :height exp-height)
              (set-rec-position exp
                                (+ sigma-right 2)
                                (+ sigma-top (/ (- sigma-height exp-height) 2) centre))
              (clim:stream-add-output-record stream (make-boxed-output-record stream exp)))
            ;;
            (when sym2
              (let ((variable (clim:with-output-to-output-record (stream)
                                (with-aligned-rendering (stream)
                                  (render-aligned () (with-italic-text-style (stream)
                                                       (render-formatted stream "d")))
                                  (render-aligned () (render-maxima-expression stream sym2))))))
                (dimension-bind (exp :right x)
                  (move-rec variable (+ x 2) 0)
                  (clim:stream-add-output-record stream variable))))))))))

(defun render-intsum (stream f var from to symbol sym2)
  (if (> (maxima::lbp *rop*) (maxima::rbp 'maxima::mparen))
      (let ((rec (clim:with-output-to-output-record (stream)
                   (render-intsum-inner stream f var from to symbol sym2))))
        (wrap-with-parens stream rec))
      (render-intsum-inner stream f var from to symbol sym2)))

(defun render-sum (stream f var from to)
  (render-intsum stream f var from to #\GREEK_CAPITAL_LETTER_SIGMA nil))

(defun render-integrate (stream f var from to)
  (render-intsum stream f nil from to #\INTEGRAL var))

(defun render-sqrt (stream expr)
  (let ((exp (clim:with-output-to-output-record (stream)
               (let ((*lop* 'maxima::mparen))
                 (render-maxima-expression stream expr)))))
    (dimension-bind (exp :height height :x x :y y :bottom bottom :right right)
      (let* ((angle 0.2)
             (hg 0.4)
             (hg-angle 0.4)
             (hg-height (* height hg)))
        (clim:draw-design stream (clim:make-polyline* (list (- x (* height angle) (* hg-height hg-angle)) (- bottom hg-height)
                                                            (- x (* height angle)) bottom
                                                            x y
                                                            right y))
                          :line-thickness 2)
        (clim:stream-add-output-record stream exp)))))

(defun render-maxima-expression (stream expr)
  (labels ((render-inner (fixed)
             (log:info "checking fixed = ~s" fixed)
             (case (caar fixed)
               (maxima::mquotient (render-quotient stream (second fixed) (third fixed)))
               (maxima::rat (render-quotient stream (second fixed) (third fixed)))
               (maxima::mplus (render-plus stream (cdr fixed)))
               (maxima::mminus (render-negation stream (second fixed)))
               (maxima::mtimes (render-times stream (cdr fixed)))
               (maxima::mexpt (render-expt stream (second fixed) (third fixed)))
               (maxima::mequal (render-equal stream (second fixed) (third fixed)))
               (maxima::%sum (render-sum stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               (maxima::%integrate (render-integrate stream (second fixed) (third fixed) (fourth fixed) (fifth fixed)))
               (maxima::%sqrt (render-sqrt stream (second fixed)))
               (t (render-function stream (car fixed) (cdr fixed))))))
    (let ((fixed (maxima::nformat-check expr)))
      (log:info "Calling render expression on: ~s (lop=~a rop=~a)" fixed *lop* *rop*)
      (etypecase fixed
        (number (render-formatted stream "~a" fixed))
        (symbol (render-symbol stream fixed))
        (list (if (or (<= (maxima::lbp (caar fixed)) (maxima::rbp *lop*))
                      (<= (maxima::rbp (caar fixed)) (maxima::lbp *rop*)))
                  (let ((output-record (clim:with-output-to-output-record (stream)
                                         (render-inner fixed))))
                    (wrap-with-parens stream output-record))
                  (render-inner fixed)))))))

(defun make-expression-output-record (stream expr)
  (log:info "Making output record for expr: ~s" expr)
  (let ((*font-size* 14))
    (with-roman-text-style (stream)
      (let ((output-record (clim:with-output-to-output-record (stream)
                             (with-paren-op
                               (render-maxima-expression stream expr)))))
        (setf (clim:output-record-position output-record) (values 0 0))
        output-record))))
