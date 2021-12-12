(cl:in-package :notalone-thriced)


(defclass end-state (notalone-state)
  ((kill-count :initarg :kill-count :initform nil)
   music))


(defmethod initialize-instance :after ((this end-state) &key kill-count)
  (with-slots (music) this
    (setf music (aw:make-audio-source (find-asset :audio "menu")))

    (update-records kill-count)

    (aw:play-audio-source music)
    (setf (aw:audio-source-looping-p music) t)))


(defmethod withdraw ((this end-state))
  (with-slots (music) this
    (aw:destroy-audio-source music)))


(defmethod react ((this end-state) event)
  (with-slots (kill-count) this
    (case (aw:event-type event)
      (:keyboard-button-down
       (case (aw:event-key-scan-code event)
         ((:return :escape :space) (transition-to 'initial-state :kill-count kill-count)))))))


(defmethod act ((this end-state))
  (with-slots (banner) this))


(defmethod draw ((this end-state))
  (with-slots (kill-count) this
    (let ((time (real-time-seconds)))
      (aw:paint-color 0.75 0.15 0)
      (aw:with-saved-transform ()
        (aw:translate 80 250)
        (aw:rotate (* (/ pi 180) 2))
        (aw:with-font (*title-typeface*)
          (aw:font-baseline-snap nil)
          (aw:font-subpixel t)
          (aw:font-size 160)
          (let ((x (* (cos (* 1 time)) 20))
                (y (* (sin (* 2 time)) 5)))
            (aw:text x y "MISSING")
            (aw:translate 380 100)
            (aw:text x y "IN ACTION"))))

      (when kill-count
        (aw:with-saved-transform ()
          (aw:translate 400 580)
          (aw:with-font (*menu-typeface*)
            (aw:font-size 22)
            (aw:text 0 0 (format nil "DROP RESULT: ~A" kill-count)))))

      (aw:with-saved-transform ()
        (menu-paint-color)
        (aw:translate 480 700)
        (aw:with-font (*menu-typeface*)
          (aw:font-size 26)
          (aw:with-saved-transform ()
            (aw:translate (* 10 (abs (sin (* 5 time)))) 0)
            (aw:scale 0.8 0.8)
            (aw:text -110 -2  ">>>"))
          (aw:text 0 0 "ESCAPE"))))))
