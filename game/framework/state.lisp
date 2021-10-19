(cl:in-package :notalone-thriced)


(declaim (special *game-state*))


(defgeneric act (state)
  (:method (state)
    (declare (ignore state))))


(defgeneric draw (state)
  (:method (state)
    (declare (ignore state))))


(defgeneric react (state event)
  (:method (state event)
    (declare (ignore state event))))


(defgeneric withdraw (state)
  (:method (state)
    (declare (ignore state))))


(defclass game-state ()
  ((state :initform nil)
   (next-state :initform nil)))


(defun game-state ()
  (with-slots (state) *game-state*
    state))


(defun game-state-process-event (event)
  (react (game-state) event))


(defun game-state-act ()
  (with-slots (state next-state) *game-state*
    (when next-state
      (withdraw state)
      (setf state (apply #'make-instance next-state)
            next-state nil))
    (act state)))


(defun game-state-draw ()
  (draw (game-state)))


(defun game-state-withdraw ()
  (withdraw (game-state)))


(defun transition-to (state-class &rest args &key &allow-other-keys)
  (with-slots (next-state) *game-state*
    (setf next-state (list* state-class args))))
