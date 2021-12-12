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
(defclass record-state (notalone-state)
  (music))


(defmethod initialize-instance :after ((this record-state) &key)
  (with-slots (music) this
    (setf music (aw:make-audio-source (find-asset :audio "menu")))

    (aw:play-audio-source music)
    (setf (aw:audio-source-looping-p music) t)))


(defmethod withdraw ((this record-state))
  (with-slots (music) this
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
          (aw:text x y "SERVICE")
          (aw:translate 220 100)
          (aw:text x y "RECORD"))))

    (menu-paint-color)
    (aw:with-saved-transform ()
      (aw:translate 200 500)
      (let ((step 60))
        (aw:with-font (*menu-typeface*)
          (aw:font-size 22)
          (loop for record in *records*
                for idx from 0 below 3
                for (date . result) = record
                do (aw:text 0 (* step idx) "~A" date)
                   (aw:text 720 (* step idx) "~A" result)))))

    (aw:with-saved-transform ()
      (aw:translate 510 800)
      (aw:with-font (*menu-typeface*)
        (aw:font-size 26)
        (aw:with-saved-transform ()
          (aw:translate (* 10 (abs (sin (* 5 time)))) 0)
          (aw:scale 0.8 0.8)
          (aw:text -110 -2  ">>>"))
        (aw:text 0 0 "RETURN")))))
