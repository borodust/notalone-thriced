(cl:in-package :notalone-thriced)

;;;
;;; PARTICLE
;;;
(defstruct (particle-cloud-instance
            (:constructor %make-particle-cloud-instance))
  instance
  created)


(defun make-particle-cloud-instance (x y angle vel-x vel-y)
  (let ((particles (make-particle-cloud *renderer* (find-asset :material "particle")
                                        x y angle vel-x vel-y)))
    (setf (particle-cloud-delta particles) 0)
    (aw:add-scene-entity *renderer* (particle-cloud-entity particles))
    (%make-particle-cloud-instance :instance particles)))


(defun update-particle-cloud-instance (instance)
  (let ((particles (particle-cloud-instance-instance instance))
        (current-time (real-time-seconds)))
    (unless (particle-cloud-instance-created instance)
      (setf (particle-cloud-instance-created instance) current-time))
    (let ((delta (- current-time
                    (particle-cloud-instance-created instance))))
      (setf (particle-cloud-delta particles) (+ 0.05
                                                (if (zerop delta)
                                                    0
                                                    (* 1/4 (log (+ 1 (* 25 delta)) 10))))))))


(defun particle-cloud-instance-life (instance)
  (if (particle-cloud-instance-created instance)
      (- (real-time-seconds) (particle-cloud-instance-created instance))
      0))


(defun destroy-particle-cloud-instance (instance)
  (let ((instance (particle-cloud-instance-instance instance)))
    (aw:remove-scene-entity *renderer* (particle-cloud-entity instance))
    (destroy-particle-cloud instance)))


;;;
;;; ENDLESS FLOOR
;;;
(defstruct (endless-floor
            (:constructor %make-endless-floor))
  (patches nil))


(defun make-endless-floor (width height)
  (let ((patches (make-array (list width height))))
    (loop for i below width
          do (loop for j below height
                   do (setf
                       (aref patches i j)
                       (make-floor *renderer*
                                   (find-asset :material "floor")
                                   (find-asset :texture "floor_baseColor")
                                   (find-asset :texture "floor_normal")
                                   (find-asset :texture "floor_arm")))
                      (aw:add-scene-entity *renderer* (floor-entity (aref patches i j)))))
    (%make-endless-floor :patches patches)))


(defun update-endless-floor (floor x y)
  (let* ((patches (endless-floor-patches floor))
         (width (array-dimension patches 0))
         (height (array-dimension patches 1))
         (offset-x (+ (/ width -2) (floor x)))
         (offset-y (+ (/ height -2) (floor y))))

    (with-transform (root-transform
                     (:translation :x offset-x :y offset-y :z 0))

      (loop for i below width
            do (loop for j below height
                     for patch = (aref patches i j)
                     do (with-transform (transform
                                         (:transform root-transform)
                                         (:translation :x i :y j))
                          (aw:transform-entity *renderer* (floor-entity patch) transform)))))))


(defun destroy-endless-floor (floor)
  (let ((patches (endless-floor-patches floor)))
    (loop for i below (array-dimension patches 0)
          do (loop for j below (array-dimension patches 1)
                   for patch = (aref patches i j)
                   do (aw:remove-scene-entity *renderer* (floor-entity patch))
                      (destroy-floor patch)))))

;;;
;;; STATE
;;;
(defclass gameplay-state ()
  ((mouse-state :initform (aw:make-mouse-state))
   (key-bag :initform (list))
   (player-angle :initform (/ pi 2))
   (player-velocity :initform (aw:make-vec2 0 0))
   (player-position :initform (aw:make-vec2 0 0))
   (particles :initform (list))
   floor flashlight))


(defmethod initialize-instance :after ((this gameplay-state) &key)
  (with-slots (floor flashlight) this
    (setf floor (make-endless-floor 5 4)
          flashlight (aw:with-vec3 (light-direction :x 0f0 :y 0f0 :z -1f0)
                       (aw:make-light *renderer* :focused-spot
                                      (aw:.cast-shadows t)
                                      (aw:.intensity 100000)
                                      (aw:.direction light-direction)
                                      (aw:.falloff 10)
                                      (aw:.spot-light-cone (/ pi 16) (/ pi 8)))))

    (aw:camera-lens-projection *renderer* 28f0 (/ *width* *height*) 0.1 100)
    (aw:add-scene-entity *renderer* flashlight)))


(defmethod withdraw ((this gameplay-state))
  (with-slots (floor flashlight particles player-velocity player-position) this

    (loop for instance in particles
          do (destroy-particle-cloud-instance instance))

    (aw:remove-scene-entity *renderer* flashlight)

    (aw:destroy-light *renderer* flashlight)
    (destroy-endless-floor floor)

    (aw:destroy-vec2 player-velocity)
    (aw:destroy-vec2 player-position)))


(defmethod react ((this gameplay-state) event)
  (with-slots (key-bag particles player-position player-angle player-velocity) this
    (let ((event-type (aw:event-type event)))
      (case event-type
        ((:keyboard-button-down :keyboard-button-up)
         (let ((pressed-p (eq  event-type :keyboard-button-down))
               (key (aw:event-key-scan-code event)))
           (case key
             (:escape (when pressed-p
                        (transition-to 'initial-state)))
             ((:w :a :s :d) (if pressed-p
                                (pushnew key key-bag)
                                (a:deletef key-bag key))))))
        (:mouse-button-down
         (when (eq :left (aw:event-mouse-button event))
           (push (make-particle-cloud-instance (aw:vec2 player-position 0)
                                               (aw:vec2 player-position 1)
                                               player-angle
                                               (aw:vec2 player-velocity 0)
                                               (aw:vec2 player-velocity 1))
                 particles)))))))


(defmethod act ((this gameplay-state))
  (with-slots (floor flashlight mouse-state key-bag particles
               player-angle player-position player-velocity)
      this
    (aw:mouse-state mouse-state)

    (loop for instance in particles
          do (update-particle-cloud-instance instance)
          when (> (particle-cloud-instance-life instance) 2)
            collect instance into for-removal
          finally (loop for instance in for-removal
                        do (destroy-particle-cloud-instance instance)
                           (a:deletef particles instance)))

    (aw:with-vec2* ((tmp :x 0 :y 0)
                    (mov-dir :x 0 :y 0))
      (loop for key in key-bag
            do (case key
                 (:w (incf (aw:vec2 tmp 1)))
                 (:a (decf (aw:vec2 tmp 0)))
                 (:s (decf (aw:vec2 tmp 1)))
                 (:d (incf (aw:vec2 tmp 0)))))
      (if (aw:vec2-equal tmp mov-dir)
          (setf (aw:vec2 player-velocity 0) 0
                (aw:vec2 player-velocity 1) 0)
          (progn
            (aw:vec2-normalize mov-dir tmp)
            (aw:vec2-scalar-mult player-velocity mov-dir 0.01)
            (aw:vec2-copy tmp player-position)
            (aw:vec2-add player-position tmp player-velocity))))

    (update-endless-floor floor
                          (aw:vec2 player-position 0)
                          (aw:vec2 player-position 1))

    (with-transform (transform
                     (:translation :x (aw:vec2 player-position 0)
                                   :y (+ (aw:vec2 player-position 1) -0.1)
                                   :z 2)
                     (:rotation (* (/ pi 180) 3) :x 1))
      (aw:transform-camera *renderer* transform))


    (aw:with-vec2* ((ref-vec :x 0 :y 1)
                    (mouse-vec :x (- (aw:mouse-state-x mouse-state) (/ *width* 2))
                               :y (- (- *height* (aw:mouse-state-y mouse-state)) (/ *height* 2)))
                    (mouse-nvec))
      (aw:vec2-normalize mouse-nvec mouse-vec)
      (let ((pre-angle (acos (aw:vec2-dot ref-vec mouse-nvec))))
        (setf player-angle (if (> (aw:vec2 mouse-vec 0) 0)
                               (- (* 2 pi) pre-angle)
                               pre-angle))))

    (with-transform (root-transform
                     (:translation :x (aw:vec2 player-position 0)
                                   :y (aw:vec2 player-position 1))
                     (:rotation player-angle :z 1))
      (with-transform (transform
                       (:transform root-transform)
                       (:translation :z 0.05)
                       (:rotation (* (/ pi 180) 80) :x 1))
        (aw:transform-entity *renderer* flashlight transform)))))


(defmethod draw ((this gameplay-state))
  (with-slots () this))
