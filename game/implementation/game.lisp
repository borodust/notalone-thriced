(cl:in-package :notalone-thriced)


(defclass initial-state ()
  ((selected :initform 0)
   canvas banner
   title-typeface
   menu-typeface))


(defmethod initialize-instance :after ((this initial-state) &key)
  (with-slots (canvas banner banner-entity title-typeface menu-typeface) this
    (setf canvas (aw:make-canvas *renderer* *width* *height*)
          banner (make-banner *renderer* (find-asset :material "banner") *width* *height*)
          title-typeface (find-asset :typeface "sector17")
          menu-typeface (find-asset :typeface "sector34"))

    (with-transform (transform
                     (:rotation 0 :x 0 :y 0 :z 1)
                     (:translation :x 0 :y 0 :z 0)
                     (:scale :x 1 :y 1 :z 1))
      (aw:transform-camera *renderer* transform))

    (aw:camera-ortho-projection *renderer* 0 *width* 0 *height*)
    (aw:add-scene-entity *renderer* (banner-entity banner))))


(defmethod withdraw ((this initial-state))
  (with-slots (canvas banner title-typeface menu-typeface) this
    (aw:remove-scene-entity *renderer* (banner-entity banner))
    (destroy-banner banner)
    (aw:destroy-canvas canvas)))


(defmethod react ((this initial-state) event)
  (with-slots (selected) this
    (flet ((next-menu-item ()
             (setf selected (mod (1+ selected) 3)))
           (prev-menu-item ()
             (setf selected (mod (1- selected) 3)))
           (perform-item-action ()
             (case selected
               (0 (transition-to 'gameplay-state))
               (2 (throw 'quit nil)))))
      (case (aw:event-type event)
        (:keyboard-button-down
         (case (aw:event-key-scan-code event)
           ((:down :s) (next-menu-item))
           ((:up :w) (prev-menu-item))
           (:return (perform-item-action))))
        (:gamepad-button-down
         (case (aw:event-gamepad-button event)
           (:dpad-down (next-menu-item))
           (:dpad-up (prev-menu-item))
           (:a (perform-item-action))))))))


(defmethod act ((this initial-state))
  (with-slots (banner) this))


(defmethod draw ((this initial-state))
  (with-slots (canvas banner title-typeface menu-typeface selected) this
    (setf (banner-texture banner) (aw:canvas-texture canvas))
    (let ((time (real-time-seconds)))
      (aw:with-canvas (canvas)
        (aw:paint-color 0.4 0.5 0)
        (aw:with-saved-transform ()
          (aw:translate 140 200)
          (aw:rotate (* (/ pi 180) 2))
          (aw:with-font (title-typeface)
            (aw:font-baseline-snap nil)
            (aw:font-subpixel t)
            (aw:font-size 160)
            (let ((x (* (cos (* 1 time)) 20))
                  (y (* (sin (* 2 time)) 5)))
              (aw:text x y "NOTALONE:")
              (aw:translate 220 100)
              (aw:text x y "THRICED"))))

        (aw:paint-color 0.6 0.5 0)
        (aw:with-saved-transform ()
          (aw:translate 160 600)
          (aw:with-font (menu-typeface)
            (aw:font-size 26)
            (let ((step 60))
              (aw:with-saved-transform ()
                (aw:translate (* 10 (abs (sin (* 5 time))))
                              (* selected step))
                (aw:scale 0.8 0.8)
                (aw:text -110 -2  ">>>"))
              (aw:text 0 (* step 0) "DROP IN")
              (aw:text 0 (* step 1) "SERVICE RECORD")
              (aw:text 0 (* step 2) "FLEE"))))))))
