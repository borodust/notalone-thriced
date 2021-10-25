(cl:in-package :notalone-thriced.tools)


(define-tool-window (eden-window "Tools" ;; :width 400 :height 600
                     )
  (when (awt:collapsing-header "Debug")
    (when (awt:button "Clear")
      (clrhash notalone-thriced::*debug-table*))
    (loop for name being the hash-key of notalone-thriced::*debug-table*
            using (hash-value value)
          do (awt:text (format nil "~A: ~A" name value)))))
