(defpackage :sdl-test
  (:use :cl)
  (:export #:simple-test #:sdl-only #:cairo-test #:custom-event-loop))

(in-package :sdl-test)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "LOG4CL")
    (ql:quickload "log4cl"))
  (unless (find-package "CLIM")
    #+sbcl
    (progn
      (sb-ext:restrict-compiler-policy 'safety 3)
      (sb-ext:restrict-compiler-policy 'debug 3))
    (ql:quickload "mcclim"))
  (unless (find-package "CAIRO")
    (ql:quickload "cl-cairo2")))

(setq climi::*server-path-search-order* '(:sdl))

(defun display-text-content (frame stream)
  (declare (ignore frame)) 
  (clim:draw-line* stream 10 10 200 50 :ink clim:+red+ :line-thickness 4)
  (clim:draw-line* stream 200 10 10 50 :ink clim:+blue+ :line-thickness 10)
  (clim:draw-rectangle* stream 30 30 100 150 :ink clim:+green+ :filled nil)
  (clim:draw-rectangle* stream 50 50 70 70 :ink clim:+green+ :filled t))

(defun display-content2 (frame stream)
  (declare (ignore frame))
  #+nil
  (progn
    (format stream "Some text here~%")
    (clim:with-text-size (stream 30)
      (format stream "This is a larger font~%")))
  (clim:draw-text* stream "Test message" 50 50 :ink clim:+red+)
  (clim:draw-line* stream 10 100 50 20))

(clim:define-application-frame foo-frame ()
  ()
  (:panes (text-content :application
                        :display-function 'display-text-content)
          (content2 (clim:make-clim-stream-pane :display-function 'display-content2
                                                :incremental-redisplay nil
                                                :display-time t)))
  (:layouts (default (clim:vertically ()
                       text-content
                       content2))))

(defun simple-test ()
  (let ((frame (clim:make-application-frame 'foo-frame
                                            :width 800
                                            :height 800)))
    (clim:run-frame-top-level frame)))

(defun sdl-only ()
  (let ((in *standard-input*))
    (sdl2:with-init (:everything)
      (let ((win (sdl2:in-main-thread ()
                   (sdl2:create-window :title "Foo"
                                       :w 400 :h 400
                                       :flags '(:shown)))))
        (sdl2:in-main-thread ()
          (sdl2:with-renderer (renderer win :flags '(:accelerated))
            (sdl2:set-render-draw-color renderer 255 255 255 255)
            (sdl2:render-clear renderer)
            (sdl2:set-render-draw-color renderer 0 0 255 255)
            (sdl2:render-draw-line renderer 10 10 700 700)
            (sdl2:render-present renderer))))
      (sdl2:with-event-loop (:method :poll)
        (:keyup (:keysym keysym)
                (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                  (sdl2:push-event :quit)))
        (:quit ()
               t))
      (format t "Waiting for newline~%")
      (read-line in)
      (format t "Finished~%"))))

(defun custom-event-loop ()
  (sdl2:with-init (:everything)
    (let* ((win (sdl2:in-main-thread ()
                 (sdl2:create-window :title "Foo"
                                     :w 400 :h 400
                                     :flags '(:shown :resizable))))
           (renderer (sdl2:create-renderer win nil '(:accelerated))))
      (sdl2:in-main-thread ()
        (sdl2:set-render-draw-color renderer 255 255 255 255)
        (sdl2:render-clear renderer)
        (sdl2:set-render-draw-color renderer 0 0 255 255)
        (sdl2:render-draw-line renderer 10 10 700 700)
        (sdl2:render-present renderer)))
    (sdl2:in-main-thread ()
      (sdl2:with-sdl-event (event)
        (loop
          for rc = (sdl2:next-event event :wait 10000)
          if (zerop rc)
            do (log:info "event timeout")
          else
            do (log:info "rc=~s  type=~s" rc (sdl2:get-event-type event)))))
    (format t "Finished~%")))

(defun draw-x (x y length)
  (cairo:move-to (- x length) (- y length))
  (cairo:line-to (+ x length) (+ y length))
  (cairo:stroke)
  (cairo:move-to (- x length) (+ y length))
  (cairo:line-to (+ x length) (- y length))
  (cairo:stroke))

(defun paint-cairo (context)
  (cairo:with-context (context)
    ;;(cairo:set-source-rgb 1 1 1)
    ;;(cairo:paint)
    (cairo:set-source-rgb 0 1 0)
    (cairo:move-to 20 20)
    (cairo:line-to 100 50)
    (cairo:stroke)
    (cairo:set-source-rgb 1 0 0)
    (cairo:rectangle 10 10 200 100)
    (cairo:stroke)
    (cairo:set-source-rgb 0 0 1)
    (cairo:rectangle 50 50 60 60)
    (cairo:fill-path)
    (cairo:set-source-rgb 0 1 0)
    (cairo:select-font-face "Source Code Pro" :normal :normal)
    (cairo:set-font-size 30)
    (cairo:move-to 100 100)
    (cairo:show-text "Some text")
    (draw-x 100 100 10)
    (log:info "Extents: ~s" (multiple-value-list (cairo:text-extents "Some text")))))

(defun draw-cairo-window (win)
  (sdl2:in-main-thread ()
    (multiple-value-bind (width height)
        (sdl2:get-window-size win)
      (log:info "Drawing with dimensions (~s,~s)" width height)
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (let ((texture (sdl2:create-texture renderer :argb8888 :streaming width height)))
          (multiple-value-bind (pixels pitch)
              (sdl2:lock-texture texture)
            (let ((surface (cairo:create-image-surface-for-data pixels :argb32 width height pitch)))
              (let ((context (cairo:create-context surface)))
                (paint-cairo context)))
            (sdl2:unlock-texture texture)
            (sdl2:render-copy renderer texture)
            (sdl2:destroy-texture texture)))
        (sdl2:render-present renderer)))))

(defun cairo-test ()
  (sdl2:with-init (:everything)
    (let ((win (sdl2:in-main-thread ()
                 (sdl2:create-window :title "Foo"
                                     :w 400 :h 400
                                     :flags '(:shown :resizable)))))
      (draw-cairo-window win))
    (sdl2:with-event-loop (:method :poll)
      (:windowevent
       (:event window-event-type :window-id window-id)
       (case window-event-type
         (#.sdl2-ffi:+sdl-windowevent-resized+
          (log:info "resized (id=~s  win=~s)"
                    window-id
                    (sdl2-ffi.functions:sdl-get-window-from-id window-id)))
         (#.sdl2-ffi:+sdl-windowevent-exposed+
          (log:info "expose")
          (draw-cairo-window (sdl2-ffi.functions:sdl-get-window-from-id window-id)))
         (t (log:info "Window event of type: ~s" window-event-type))))
      (:mousemotion
       (:window-id window-id :x x :y y :xrel xrel :yrel yrel :state state)
       (log:info "Mouse: pos:(~s,~s) rel:(~s,~s) state:~s win:~s"
                 x y xrel yrel state window-id))
      (:keyup
       (:keysym keysym)
       (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
         (sdl2:push-event :quit)))
      (:quit
       ()
       t))))
