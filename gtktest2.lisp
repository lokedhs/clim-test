(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (progn
    (sb-ext:restrict-compiler-policy 'safety 3)
    (sb-ext:restrict-compiler-policy 'debug 3))
  #+nil
  (unless (find-package "CLIM")
    (ql:quickload "mcclim"))
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (unless (find-package "GTK")
    (ql:quickload "cl-cffi-gtk")))

(defpackage :gtk-example
  (:use :gtk :gdk :gdk-pixbuf :gobject :glib :gio :pango :cairo :common-lisp))

(in-package :gtk-example)

;; The cl-cffi-gtk documentation and tutorial are both very good;
;; https://www.crategus.com/books/cl-cffi-gtk/
;; https://www.crategus.com/books/cl-gtk/gtk-tutorial.html

(defvar *should-quit-main* nil)

(defun example-window ()
  (within-main-loop
    (let ((window (make-instance 'gtk-window
				 :type :toplevel
				 :title "cl-cffi-gtk Example"
				 :default-width 500
				 :default-height 500)))
      (g-signal-connect window "destroy"
			(lambda (widget)
			  (declare (ignore widget))
			  (leave-gtk-main)))
      (g-signal-connect window "delete-event"
			(lambda (widget event)
			  (declare (ignore widget event))
			  (if *should-quit-main*
			      +gdk-event-propagate+
			      (progn (setf *should-quit-main* t)
				     +gdk-event-stop+))))

      (let ((drawing-area (make-instance 'gtk-drawing-area)))
	(g-signal-connect drawing-area "draw"
			  (lambda (widget cr)
			    (declare (ignore widget))
			    (let ((cr (pointer cr)))
			      (cairo-set-source-rgb cr 1 1 1)
			      (cairo-paint cr)
			      
			      (cairo-arc cr 150 100 50 0 (* 2 pi))
			      (cairo-set-source-rgb cr 0 0 0)
			      (cairo-fill-preserve cr)
			      (cairo-set-source-rgb cr 1 1 1)
			      (cairo-stroke cr)

			      (cairo-move-to cr 50 200)
			      (cairo-set-source-rgb cr 1 0 0.2)
			      (cairo-line-to cr 100 220)
			      (cairo-stroke cr)
			      (cairo-move-to cr 250 250)
			      (cairo-set-source-rgb cr 0.5 1 0.5)
			      (cairo-rel-line-to cr 0 50)
			      (cairo-rel-line-to cr -50 0)
			      (cairo-rel-curve-to cr -50 -100 -50 150 -50 0)
			      (cairo-close-path cr)
			      (cairo-fill cr))
			    t))
	
	(gtk-container-add window drawing-area))

      (g-signal-connect window "button-press-event"
			(lambda (widget event)
			  (declare (ignore widget event))
			  (format t "Press~%")
			  +gdk-event-stop+))
      (g-signal-connect window "button-release-event"
			(lambda (widget event)
			  (declare (ignore widget event))
			  (format t "Release~%")
			  +gdk-event-stop+))

      (g-signal-connect window "key-press-event"
			(lambda (widget event)
			  (declare (ignore widget event))
			  (format t "Key Down~%")
			  +gdk-event-stop+))
      (g-signal-connect window "key-release-event"
			(lambda (widget event)
			  (declare (ignore widget event))
			  (format t "Key Up~%")
			  +gdk-event-stop+))
      

      ;; Movement handling can be tricky because GTK will suppress most events unless we really ask
      ;; for them. See https://developer.gnome.org/gtk-tutorial/stable/x2431.html for details.
      (g-signal-connect window "motion-notify-event"
			(lambda (widget event)
			  (declare (ignore widget event))
			  (format t "Motion~%")
			  +gdk-event-stop+))
      (gtk-widget-add-events window '(:all-events-mask))
      
      (gtk-widget-show-all window))))

(export 'example-window)

(defun alternate-main-loop ()
  ;; Creates an idle source and attaches it, these can be done separately with more control if
  ;; required.
  (g-idle-add (lambda ()
		(let ((window (make-instance 'gtk-window
					     :title "Explicitly Enter Main Loop")))
		  (g-signal-connect window "destroy"
				    (lambda (w)
				      (declare (ignore w))
				      (leave-gtk-main)))
		  (gtk-widget-show-all window))
		;; Removes 
		+g-source-remove+))
  ;; An alternative `ensure-gtk-main' will start the main loop in a new thread if none is currently
  ;; running. That is what `within-main-loop' uses.
  (gtk-main))

(export 'alternate-main-loop)
