(cl:in-package :notalone-thriced)


(defvar *records* nil)


(defun update-records (kill-count)
  (when kill-count
    (let ((top-result (first *records*)))
      (when (or (null top-result)
                (> kill-count (cdr top-result)))
        (push (cons (local-time:format-timestring nil (local-time:now)
                                                  :format local-time:+asctime-format+)
                    kill-count)
              *records*)))))

;;;
;;; STATE
;;;
(defclass record-state ()
  (canvas banner
   title-typeface
   menu-typeface
   music))


(defmethod initialize-instance :after ((this record-state) &key)
  (with-slots (canvas banner title-typeface menu-typeface music) this
    (setf canvas (aw:make-canvas *renderer* *width* *height*)
          banner (make-banner *renderer* (find-asset :material "banner") *width* *height*)
          title-typeface (find-asset :typeface "sector17")
          menu-typeface (find-asset :typeface "sector34")
          music (aw:make-audio-source (find-asset :audio "menu")))

    (aw:play-audio-source music)
    (setf (aw:audio-source-looping-p music) t)

    (with-transform (transform
                     (:rotation 0 :x 0 :y 0 :z 1)
                     (:translation :x 0 :y 0 :z 0)
                     (:scale :x 1 :y 1 :z 1))
      (aw:transform-camera *renderer* transform))

    (aw:camera-ortho-projection *renderer* 0 *width* 0 *height*)
    (aw:add-scene-entity *renderer* (banner-entity banner))))


(defmethod withdraw ((this record-state))
  (with-slots (canvas banner title-typeface menu-typeface music) this
    (aw:remove-scene-entity *renderer* (banner-entity banner))
    (destroy-banner banner)
    (aw:destroy-canvas canvas)
    (aw:destroy-audio-source music)))


(defmethod react ((this record-state) event)
  (with-slots () this
    (case (aw:event-type event)
      (:keyboard-button-down
       (case (aw:event-key-scan-code event)
         ((:return :escape :space) (transition-to 'initial-state)))))))


(defmethod act ((this record-state))
  (with-slots (banner) this))


(defmethod draw ((this record-state))
  (with-slots (canvas banner title-typeface menu-typeface) this
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
              (aw:text x y "SERVICE")
              (aw:translate 220 100)
              (aw:text x y "RECORD"))))

        (aw:paint-color 0.6 0.5 0)
        (aw:with-saved-transform ()
          (aw:translate 200 500)
          (let ((step 60))
            (aw:with-font (menu-typeface)
              (aw:font-size 22)
              (loop for record in *records*
                    for idx from 0 below 3
                    for (date . result) = record
                    do (aw:text 0 (* step idx) "~A" date)
                       (aw:text 720 (* step idx) "~A" result)))))

        (aw:with-saved-transform ()
          (aw:translate 510 800)
          (aw:with-font (menu-typeface)
            (aw:font-size 26)
            (aw:with-saved-transform ()
              (aw:translate (* 10 (abs (sin (* 5 time)))) 0)
              (aw:scale 0.8 0.8)
              (aw:text -110 -2  ">>>"))
            (aw:text 0 0 "RETURN")))))))
