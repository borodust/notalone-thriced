(cl:in-package :notalone-thriced)


(declaim (special *game-state*
                  *state*))


(defgeneric act (state)
  (:method (state)
    (declare (ignore state))))


(defmethod act :around (state)
  (let ((*state* state))
    (call-next-method)))


(defgeneric draw (state)
  (:method (state)
    (declare (ignore state))))


(defmethod draw :around (state)
  (let ((*state* state))
    (call-next-method)))


(defgeneric react (state event)
  (:method (state event)
    (declare (ignore state event))))


(defmethod react :around (state event)
  (declare (ignore event))
  (let ((*state* state))
    (call-next-method)))


(defgeneric withdraw (state)
  (:method (state)
    (declare (ignore state))))


(defmethod withdraw :around (state)
  (let ((*state* state))
    (call-next-method)))


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
