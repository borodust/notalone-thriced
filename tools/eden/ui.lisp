(cl:in-package :notalone-thriced.tools)


(define-tool-window (eden-window "Tools" ;; :width 400 :height 600
                     )
  (when (awt:collapsing-header "Info")
    (when (awt:button "Clear")
      (clrhash notalone-thriced::*debug-table*))
    (loop for name being the hash-key of notalone-thriced::*debug-table*
            using (hash-value value)
          do (awt:text (format nil "~A: ~A" name value))))
  (when (awt:collapsing-header "Utilities")
    (let ((gain (aw:audio-listener-gain)))
      (when (awt:checkbox "Mute Audio" (= gain 0))
        (setf (aw:audio-listener-gain) (if (> gain 0) 0 1))))))
