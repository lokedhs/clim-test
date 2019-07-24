(defpackage :polygon-triangles
  (:use :cl)
  (:export #:open-foo-frame))

(in-package :polygon-triangles)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "CLIM")
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3)
    (unless (find-package "LOG4CL")
      (ql:quickload "log4cl"))
    (ql:quickload "mcclim")))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun and-there-it-is--my-polygon ()
  (let ((path (paths:stroke-path (let ((p (paths:create-path :open-polyline)))
                                   (paths:path-reset p (paths:make-point 10 20))
                                   (paths:path-extend p (paths:make-straight-line)
                                                      (paths:make-point 50 20))
                                   (paths:path-extend p (paths:make-straight-line)
                                                      (paths:make-point 40 60))
                                   (paths:path-extend p (paths:make-straight-line)
                                                      (paths:make-point 50 50))
                                   p)
                                 10 :caps :round :joint :miter)))
    ;; PATH-ITERATOR can be replaced with PATH-ITERATOR-SEGMENTED
    ;; which will return a series of line segments instead of arcs
    (loop with iterator = (paths:path-iterator-segmented (car path))
       for e = (multiple-value-list (paths:path-iterator-next iterator))
       collect (subseq e 0 2)
       until (third e))))

(defun lexicographically-< (a b)
  "Predicate for sorting in lexicographic order."
  (or (< (car a) (car b))
      (and (= (car a) (car b))
           (< (cdr a) (cdr b)))))

(defun min-point (a b &optional (order-fn #'lexicographically-<))
  (if (funcall order-fn a b) a b))

(defun point-norm (p)
  (sqrt (+ (expt (car p) 2)
           (expt (cdr p) 2))))

(defun normalized (p)
  (let ((inv-norm (/ 1.0 (point-norm p))))
    (cons (* inv-norm (car p))
          (* inv-norm (cdr p)))))

(defun point-arithmetic (fn a b)
  (cons (funcall fn (car a) (car b))
        (funcall fn (cdr a) (cdr b))))

(defun point-+ (a b) (point-arithmetic #'+ a b))
(defun point-- (a b) (point-arithmetic #'- a b))

(defun points-+ (&rest points) (reduce #'point-+ points))

(defun point-scale (p s)
  (cons (* (car p) s) (* (cdr p) s)))

(defun cross-product-2d (a b)
  (let ((x1 (car a)) (y1 (cdr a))
        (x2 (car b)) (y2 (cdr b)))
    (- (* x1 y2) (* x2 y1))))

(defun point-perp (p)
  (cons (- (cdr p)) (car p)))

(defun left-of-line-p (point line-start line-end)
  "Is POINT left of the line from LINE-START through LINE-END?"
  (plusp (cross-product-2d (point-- line-start point)
                           (point-- line-end point))))

;;; jd's code
(defun region-contains-position-p (polygon x y)
  (flet ((is-left (x0 y0 x1 y1 x2 y2)
           (- (* (- x1 x0) (- y2 y0))
              (* (- x2 x0) (- y1 y0)))))
    (let ((wn 0))
      (loop for points on (cons (car (last polygon)) polygon)
         for p1 = (car points)
         for p2 = (cadr points) 
         for x1 = (car p1) for y1 = (cdr p1)
         for x2 = (car p2) for y2 = (cdr p2)
         while (cdr points)
         do
           (if (<= y1 y)
               (when (and (> y2 y)
                          (> (is-left x1 y1 x2 y2 x y) 0))
                 (incf wn))
               (when (and (<= y2 y)
                          (< (is-left x1 y1 x2 y2 x y) 0))
                 (decf wn))))
      (values (= wn 0) wn))))

(defun find-non-simple-segments (polygon)
  (flet ((checkpoint (pt-a pt-b)
           (let* ((v-delta (point-- pt-b pt-a))
                  (midpoint (point-+
                             (point-scale v-delta 1/2)
                             pt-a))
                  (v-perp-normalized (normalized (point-perp v-delta))))
             (point-+ midpoint (point-scale v-perp-normalized 0.1)))))
    (loop
       for pts on polygon
       for pt-a = (car pts)
       for pt-b = (cadr pts)
       while pt-b
       for checkpoint = (checkpoint pt-a pt-b)
       when (not (region-contains-position-p polygon (car checkpoint) (cdr checkpoint)))
       collect (subseq pts 0 2))))

(defun triangulate-polygon (outline-path)
  "Naive implementation for triangulating a path, that is, a polygonal outline
of a pen-stroked polyline, as cl-vectors generates it:
 :STRAIGHT-LINE (5.002131984792475d0 . 20.145997611506445d0)
 :STRAIGHT-LINE (5.0727513500576995d0 . 19.150164285498796d0)
 :STRAIGHT-LINE (5.130761845609023d0 . 21.136010473465433d0)
 :STRAIGHT-LINE (5.339804570163868d0 . 18.188211227616634d0)
 ..."
  (let* ((polygon (if outline-path
                      (loop with iterator = (paths:path-iterator-segmented outline-path)
                         for e = (multiple-value-list (paths:path-iterator-next iterator))
                         collect (subseq e 0 2)
                         until (third e))
                      (and-there-it-is--my-polygon)))
         (p (mapcar #'second polygon))
         (p (cons (car (last p)) p))
         (minelt (reduce #'min-point p))
         (minpos (position minelt p))
         (p (append (subseq p minpos) (subseq p 0 minpos)))
         (q (reverse p)))
    ;; throughout this, we extend the triangle mesh by connecting a point of the
    ;; polygon to an advancing line. P and Q are the 'upper'- and 'lower'-first
    ;; parts of the boundary, as seen from the lexicographic minimum.
    (loop
       with triangles = (list)
       with lower = (rest p)
       with upper = (butlast q)
       with hp = (car upper)
       with lp = (pop lower)
       with prefer-lower = T
       while (and lower upper
                  (not (eq (car upper) (car lower))))
       if (and (or prefer-lower
                   (left-of-line-p (first upper) hp lp))
               (left-of-line-p (first lower) lp hp))
       do
         (push (list lp (first lower) hp) triangles)
         (setf lp (pop lower)
               prefer-lower nil)
       else do
         (push (list hp lp (first upper)) triangles)
         (setf hp (pop upper)
               prefer-lower T)

       finally (return triangles))))

(defun dump-triangles-to-file (triangles &optional (pathname "/tmp/tris.dat"))
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (loop for triangle in triangles do
         (loop for point in triangle do
              (format s "~f ~f~%" (car point) (cdr point)))
         (format s "~f ~f~%" (caar triangle) (cdar triangle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass text-content-view (clim:view)
  ())

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :default-view 'text-content-view
                        :display-function 'display-text-content))
  (:layouts (default (clim:vertically ()
                       text-content))))

(defun open-foo-frame ()
  (let ((frame (clim:make-application-frame 'foo-frame :width 700 :height 800)))
    (clim:run-frame-top-level frame)))

(defun display-text-content (frame stream)
  (declare (ignore frame))
  (let ((path (paths:stroke-path (let ((p (paths:create-path :open-polyline)))
                                   (paths:path-reset p (paths:make-point 10 20))
                                   (paths:path-extend p (paths:make-straight-line) (paths:make-point 200 20))
                                   (paths:path-extend p (paths:make-straight-line) (paths:make-point 180 50))
                                   p)
                                 15 :caps :round :joint :miter)))
    (let ((result (triangulate-polygon (car path))))
      (loop
        with colours = (coerce (list clim:+red+ clim:+green+ clim:+blue+ clim:+orange+ clim:+black+) 'simple-vector)
        for colour-index = 0 then (mod (1+ colour-index) (length colours))
        for ((x1 . y1) (x2 . y2) (x3 . y3)) in result
        do (clim:draw-polygon* stream (list x1 y2 x2 y2 x3 y3) :ink (aref colours colour-index))))))
