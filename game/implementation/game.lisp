(cl:in-package :notalone-thriced)


(defclass initial-state () ())


(defmethod react ((this initial-state) event)
  (case (aw:event-type event)))
