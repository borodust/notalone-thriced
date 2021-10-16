(cl:in-package :notalone-thriced)


(defun init-loop ()
  (setf (aw:skybox *renderer*) (aw:make-color-skybox *renderer* 0.1 0.1 0.12 1.0))
  (init-tools *tools*))


(defun destroy-loop ()
  (destroy-tools *tools*))


(defun handle-event (event)
  (when event
    (case (aw:event-type event)
      (:quit (throw 'quit nil)))
    (handle-tool-event *tools* event)))


(defun handle-loop ()
  (flet ((%handle-event (event)
           (handle-event event)))
    (aw:handle-events #'%handle-event))

  (let ((time-delta 0.014))
    (update-tools *tools* time-delta)
    (aw:in-frame (*renderer*)
      (render-tools *tools*))
    ;; FIXME: add proper delta calc
    (sleep time-delta)))


(defun run ()
  (handler-bind ((serious-condition (lambda (c)
                                      (format *error-output* "~%Unhandled serious condition:~%")
                                      (dissect:present c *error-output*))))
    (dissect:with-capped-stack ()
      (float-features:with-float-traps-masked t
        (shout "Initializing host")
        (aw:with-window (win :context context)
          (let* ((width (aw:window-width win))
                 (height (aw:window-height win)))
            (shout "Initializing audio")
            (aw:with-audio ()
              (shout "Initializing renderer")
              (aw:with-engine (renderer :surface (aw:window-surface win)
                                        :shared-context context
                                        :width width
                                        :height height)
                (shout "Framework ready")
                (let ((*renderer* renderer))
                  (with-tools (:notalone-thriced-tools :renderer renderer)
                    (init-loop)
                    (shout "Game ready")
                    (unwind-protect
                         (catch 'quit
                           (shout "Looping")
                           (loop
                             (tagbody start
                                (restart-case
                                    (handle-loop)
                                  (restart-loop ()
                                    :report "Restart game loop"
                                    (go start))))))
                      (destroy-loop))))))))))))


(defun asset-path (asset-name)
  (cond
    ((member :android *features*) asset-name)
    ((member :appimage *features*) (merge-pathnames asset-name (merge-pathnames
                                                                "usr/share/app/"
                                                                (aw:working-directory))))
    ((or (member :msix *features*)
         (member :awd-archive *features*))
     (merge-pathnames asset-name (merge-pathnames
                                  "rsc/"
                                  (aw:working-directory))))
    (t (asdf:system-relative-pathname :notalone-thriced
                                      (merge-pathnames asset-name "assets/")))))

(aw:definit main ()
  (reload-foreign-libraries)
  (run))
