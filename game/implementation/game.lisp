(cl:in-package :notalone-thriced)


(defclass initial-state (notalone-state)
  ((selected :initform 0)
   (kill-count :initarg :kill-count :initform nil)
   music))


(defmethod initialize-instance :after ((this initial-state) &key kill-count)
  (with-slots (music) this
    (setf music (aw:make-audio-source (find-asset :audio "menu")))

    (update-records kill-count)

    (aw:play-audio-source music)
    (setf (aw:audio-source-looping-p music) t)))


(defmethod withdraw ((this initial-state))
  (with-slots (music) this
    (aw:destroy-audio-source music)))


(defmethod react ((this initial-state) event)
  (with-slots (selected) this
    (flet ((next-menu-item ()
             (setf selected (mod (1+ selected) 3)))
           (prev-menu-item ()
             (setf selected (mod (1- selected) 3)))
           (perform-item-action ()
             (case selected
               (0 (transition-to 'gameplay-state))
               (1 (transition-to 'record-state))
               (2 (throw 'quit nil)))))
      (case (aw:event-type event)
        (:keyboard-button-down
         (case (aw:event-key-scan-code event)
           ((:down :s) (next-menu-item))
           ((:up :w) (prev-menu-item))
           ((:return :space) (perform-item-action))))
        (:gamepad-button-down
         (case (aw:event-gamepad-button event)
           (:dpad-down (next-menu-item))
           (:dpad-up (prev-menu-item))
           (:a (perform-item-action))))))))


(defmethod act ((this initial-state))
  (with-slots (banner) this))


(defmethod draw ((this initial-state))
  (with-slots (selected kill-count) this
    (let ((time (real-time-seconds)))
      (title-paint-color)
      (aw:with-saved-transform ()
        (aw:translate 140 200)
        (aw:rotate (* (/ pi 180) 2))
        (aw:with-font (*title-typeface*)
          (aw:font-baseline-snap nil)
          (aw:font-subpixel t)
          (aw:font-size 160)
          (let ((x (* (cos (* 1 time)) 20))
                (y (* (sin (* 2 time)) 5)))
            (aw:text x y "NOTALONE:")
            (aw:translate 220 100)
            (aw:text x y "THRICED"))))

     (menu-paint-color)
      (aw:with-saved-transform ()
        (aw:translate 160 600)
        (aw:with-font (*menu-typeface*)
          (aw:font-size 26)
          (let ((step 60))
            (aw:with-saved-transform ()
              (aw:translate (* 10 (abs (sin (* 5 time))))
                            (* selected step))
              (aw:scale 0.8 0.8)
              (aw:text -110 -2  ">>>"))
            (aw:text 0 (* step 0) "DROP IN")
            (aw:text 0 (* step 1) "SERVICE RECORD")
            (aw:text 0 (* step 2) "FLEE"))))

      (when kill-count
        (aw:with-saved-transform ()
          (aw:translate 720 880)
          (aw:with-font (*menu-typeface*)
            (aw:font-size 22)
            (aw:text 0 0 (format nil "LAST DROP RESULT: ~A" kill-count))))))))
