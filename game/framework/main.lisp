(cl:in-package :notalone-thriced)


(defvar *assets* (make-hash-table :test 'equal))


(defun find-asset (type name)
  (gethash (list type name) *assets*))


(defun init-loop (baked-assets &rest rest-assets)
  (let* ((resources (load-resources baked-assets))
         (forged (forge-resources resources)))
    (loop for (type name asset) in (append forged rest-assets)
          do (setf
              (gethash (list type name) *assets*)
              (case type
                (:typeface (aw:make-typeface (a:read-file-into-byte-vector asset)))
                (t asset)))))

  (setf (aw:skybox *renderer*) (aw:make-color-skybox *renderer* 0.1 0.1 0.12 1.0))
  (init-tools *tools*)
  (transition-to 'initial-state))


(defun destroy-loop ()
  (game-state-withdraw)
  (loop for (type nil) being the hash-key of *assets* using (hash-value asset)
        do (case type
             (:typeface (aw:destroy-typeface asset)))))


(defun handle-event (event)
  (when event
    (case (aw:event-type event)
      (:quit (throw 'quit nil))
      (:gamepad-added (aw:grab-gamepad (aw:event-gamepad-id event))))
    (game-state-process-event event)
    (handle-tool-event *tools* event)))


(defun handle-loop ()
  (flet ((%handle-event (event)
           (handle-event event)))
    (aw:handle-events #'%handle-event))

  (let ((time-delta 0.014))
    (update-tools *tools* time-delta)
    (game-state-act)
    (aw:with-frame (*renderer*)
      (render-tools *tools*)
      (game-state-draw))
    ;; FIXME: add proper delta calc
    (sleep time-delta)))


(defun run (assets &rest rest-assets)
  (handler-bind ((serious-condition (lambda (c)
                                      (format *error-output* "~%Unhandled serious condition:~%")
                                      (dissect:present c *error-output*))))
    (dissect:with-capped-stack ()
      (float-features:with-float-traps-masked t
        (shout "Initializing host")
        (aw:with-window (win)
          (let* ((*width* (aw:window-width win))
                 (*height* (aw:window-height win)))
            (shout "Initializing audio")
            (aw:with-audio ()
              (shout "Initializing renderer")
              (aw:with-engine (renderer :window win
                                        :width *width*
                                        :height *height*)
                (shout "Framework ready")
                (let ((*renderer* renderer)
                      (*game-state* (make-instance 'game-state)))
                  (with-tools (:notalone-thriced-tools :renderer renderer)
                    (apply #'init-loop assets rest-assets)
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
  (run (asset-path "assets.bin")
       `(:typeface "sector17" ,(asset-path "sector_017.ttf"))
       `(:typeface "sector34" ,(asset-path "sector_034.otf"))))
