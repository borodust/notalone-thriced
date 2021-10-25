(cl:in-package :notalone-thriced)


(defclass end-state ()
  ((kill-count :initarg :kill-count :initform nil)
   canvas banner
   title-typeface
   menu-typeface
   music))


(defmethod initialize-instance :after ((this end-state) &key kill-count)
  (with-slots (canvas banner title-typeface menu-typeface music) this
    (setf canvas (aw:make-canvas *renderer* *width* *height*)
          banner (make-banner *renderer* (find-asset :material "banner") *width* *height*)
          title-typeface (find-asset :typeface "sector17")
          menu-typeface (find-asset :typeface "sector34")
          music (aw:make-audio-source (find-asset :audio "menu")))

    (update-records kill-count)

    (aw:play-audio-source music)
    (setf (aw:audio-source-looping-p music) t)

    (with-transform (transform
                     (:rotation 0 :x 0 :y 0 :z 1)
                     (:translation :x 0 :y 0 :z 0)
                     (:scale :x 1 :y 1 :z 1))
      (aw:transform-camera *renderer* transform))

    (aw:camera-ortho-projection *renderer* 0 *width* 0 *height*)
    (aw:add-scene-entity *renderer* (banner-entity banner))))


(defmethod withdraw ((this end-state))
  (with-slots (canvas banner title-typeface menu-typeface music) this
    (aw:remove-scene-entity *renderer* (banner-entity banner))
    (destroy-banner banner)
    (aw:destroy-canvas canvas)
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
  (with-slots (canvas banner title-typeface menu-typeface kill-count) this
    (setf (banner-texture banner) (aw:canvas-texture canvas))
    (let ((time (real-time-seconds)))
      (aw:with-canvas (canvas)
        (aw:paint-color 0.8 0.03 0)
        (aw:with-saved-transform ()
          (aw:translate 80 250)
          (aw:rotate (* (/ pi 180) 2))
          (aw:with-font (title-typeface)
            (aw:font-baseline-snap nil)
            (aw:font-subpixel t)
            (aw:font-size 160)
            (let ((x (* (cos (* 1 time)) 20))
                  (y (* (sin (* 2 time)) 5)))
              (aw:text x y "MISSING")
              (aw:translate 380 100)
              (aw:text x y "IN ACTION"))))

        (aw:paint-color 0.6 0.5 0)
        (aw:with-saved-transform ()
          (aw:translate 480 700)
          (aw:with-font (menu-typeface)
            (aw:font-size 26)
            (aw:with-saved-transform ()
              (aw:translate (* 10 (abs (sin (* 5 time)))) 0)
              (aw:scale 0.8 0.8)
              (aw:text -110 -2  ">>>"))
            (aw:text 0 0 "ESCAPE")))
        (when kill-count
          (aw:paint-color 0.8 0.03 0)
          (aw:with-saved-transform ()
            (aw:translate 400 580)
            (aw:with-font (menu-typeface)
              (aw:font-size 22)
              (aw:text 0 0 (format nil "DROP RESULT: ~A" kill-count)))))))))
